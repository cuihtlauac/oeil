type t = {
  web : Web.t;
  token : string option;
  api : Uri.t;
  etags : (string, string * Yojson.Safe.t) Hashtbl.t;
      (** path -> ETag and body: a conditional request answered 304 does not count against
          the rate limit *)
}

let make ?token web =
  { web; token; api = Uri.of_string "https://api.github.com"; etags = Hashtbl.create 32 }

let token_from_env () =
  let env k =
    match Sys.getenv_opt k with Some v when String.trim v <> "" -> Some v | _ -> None
  in
  match env "GITHUB_TOKEN" with
  | Some t -> Some t
  | None -> (
      match env "GH_TOKEN" with
      | Some t -> Some t
      | None -> (
          (* Fall back to the gh CLI, silently. *)
          try
            let ic = Unix.open_process_args_in "gh" [| "gh"; "auth"; "token" |] in
            let tok = try String.trim (input_line ic) with End_of_file -> "" in
            match Unix.close_process_in ic with
            | Unix.WEXITED 0 when tok <> "" -> Some tok
            | _ -> None
          with _ -> None))

let get_json t path =
  let headers =
    ("Accept", "application/vnd.github+json")
    :: ("X-GitHub-Api-Version", "2022-11-28")
    ::
    (match t.token with
    | Some tok -> [ ("Authorization", "Bearer " ^ tok) ]
    | None -> [])
  in
  let cached = Hashtbl.find_opt t.etags path in
  let headers =
    match cached with
    | Some (etag, _) -> ("If-None-Match", etag) :: headers
    | None -> headers
  in
  match Web.get_full ~headers t.web (Uri.of_string (Uri.to_string t.api ^ path)) with
  | Error _ as e -> e
  | Ok (304, _, _) -> (
      match cached with
      | Some (_, j) -> Ok j
      | None -> Error ("GitHub " ^ path ^ ": 304 without a cached reply"))
  | Ok (200, h, body) -> (
      try
        let j = Yojson.Safe.from_string body in
        (match Http.Header.get h "etag" with
        | Some e -> Hashtbl.replace t.etags path (e, j)
        | None -> ());
        Ok j
      with Yojson.Json_error e -> Error e)
  | Ok (s, _, body) ->
      let msg =
        try
          Yojson.Safe.from_string body
          |> Yojson.Safe.Util.member "message"
          |> Yojson.Safe.Util.to_string
        with _ -> ""
      in
      Error (Printf.sprintf "GitHub %s: HTTP %d %s" path s msg)

type pull = {
  number : int;
  title : string;
  merged : bool;
  merge_commit : string option;
  base : string;
  merged_at : string option;
  state : string;
}

let pull t ~repo ~number =
  let open Yojson.Safe.Util in
  Result.bind
    (get_json t (Printf.sprintf "/repos/%s/pulls/%d" repo number))
    (fun j ->
      try
        Ok
          {
            number;
            title = j |> member "title" |> to_string;
            merged = j |> member "merged" |> to_bool_option |> Option.value ~default:false;
            merge_commit = j |> member "merge_commit_sha" |> to_string_option;
            base = j |> member "base" |> member "ref" |> to_string;
            merged_at = j |> member "merged_at" |> to_string_option;
            state = j |> member "state" |> to_string;
          }
      with Type_error (m, _) -> Error ("unexpected GitHub reply: " ^ m))

let commit t ~repo ~ref_ =
  let open Yojson.Safe.Util in
  Result.bind
    (get_json t (Printf.sprintf "/repos/%s/commits/%s" repo ref_))
    (fun j ->
      try Ok (j |> member "sha" |> to_string)
      with Type_error (m, _) -> Error ("unexpected GitHub reply: " ^ m))

let includes t ~repo ~commit ~newer =
  let open Yojson.Safe.Util in
  Result.bind
    (get_json t (Printf.sprintf "/repos/%s/compare/%s...%s" repo commit newer))
    (fun j ->
      match j |> member "status" |> to_string_option with
      | Some ("ahead" | "identical") -> Ok true
      | Some _ -> Ok false
      | None -> Error "unexpected GitHub compare reply")

let head t ~repo ~branch = commit t ~repo ~ref_:("heads/" ^ branch)

let to_commit j : Oeil.Repo_state.commit =
  let open Yojson.Safe.Util in
  let message = j |> member "commit" |> member "message" |> to_string in
  let first =
    match String.index_opt message '\n' with
    | Some i -> String.sub message 0 i
    | None -> message
  in
  {
    sha = j |> member "sha" |> to_string;
    parents =
      j |> member "parents" |> to_list
      |> List.map (fun p -> p |> member "sha" |> to_string);
    message = first;
    date =
      (try j |> member "commit" |> member "committer" |> member "date" |> to_string
       with _ -> "");
  }

(* Commits in [head] but not in [base], oldest first (GitHub caps the list
   at 250). *)
let compare_commits t ~repo ~base ~head =
  let open Yojson.Safe.Util in
  Result.bind
    (get_json t (Printf.sprintf "/repos/%s/compare/%s...%s?per_page=250" repo base head))
    (fun j ->
      try
        Ok
          ( j |> member "status" |> to_string,
            j |> member "commits" |> to_list |> List.map to_commit )
      with Type_error (m, _) -> Error ("unexpected GitHub compare reply: " ^ m))

(* Merged pull requests into [base], most recently updated first. *)
let merged_pulls t ~repo ~base ?(per_page = 100) () =
  let open Yojson.Safe.Util in
  Result.bind
    (get_json t
       (Printf.sprintf
          "/repos/%s/pulls?state=closed&base=%s&sort=updated&direction=desc&per_page=%d"
          repo base per_page))
    (fun j ->
      try
        Ok
          (j |> to_list
          |> List.filter_map (fun p ->
              match
                ( p |> member "merged_at" |> to_string_option,
                  p |> member "merge_commit_sha" |> to_string_option )
              with
              | Some merged_at, Some merge_commit ->
                  Some
                    {
                      Oeil.Repo_state.number = p |> member "number" |> to_int;
                      title = p |> member "title" |> to_string;
                      merge_commit;
                      merged_at;
                      author =
                        (try p |> member "user" |> member "login" |> to_string
                         with _ -> "");
                    }
              | _ -> None))
      with Type_error (m, _) -> Error ("unexpected GitHub pulls reply: " ^ m))

let has_token t = Option.is_some t.token

(* Merged pull requests into [base] that contain commit [sha]. *)
let pulls_of_commit t ~repo ~sha ~base =
  let open Yojson.Safe.Util in
  Result.bind
    (get_json t (Printf.sprintf "/repos/%s/commits/%s/pulls" repo sha))
    (fun j ->
      try
        Ok
          (j |> to_list
          |> List.filter_map (fun p ->
              match
                ( p |> member "merged_at" |> to_string_option,
                  p |> member "merge_commit_sha" |> to_string_option,
                  p |> member "base" |> member "ref" |> to_string_option )
              with
              | Some merged_at, Some merge_commit, Some b when b = base ->
                  Some
                    {
                      Oeil.Repo_state.number = p |> member "number" |> to_int;
                      title = p |> member "title" |> to_string;
                      merge_commit;
                      merged_at;
                      author =
                        (try p |> member "user" |> member "login" |> to_string
                         with _ -> "");
                    }
              | _ -> None))
      with Type_error (m, _) -> Error ("unexpected GitHub reply: " ^ m))

let token t = t.token
let web t = t.web

(* A commit, with its date and message. *)
let commit_info t ~repo ~ref_ =
  Result.bind
    (get_json t (Printf.sprintf "/repos/%s/commits/%s" repo ref_))
    (fun j ->
      try Ok (to_commit j)
      with Yojson.Safe.Util.Type_error (m, _) -> Error ("unexpected GitHub reply: " ^ m))
