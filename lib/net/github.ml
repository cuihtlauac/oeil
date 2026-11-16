type t = { web : Web.t; token : string option; api : Uri.t }

let make ?token web = { web; token; api = Uri.of_string "https://api.github.com" }

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
  match Web.get ~headers t.web (Uri.with_path t.api path) with
  | Error _ as e -> e
  | Ok (200, body) -> (
      try Ok (Yojson.Safe.from_string body) with Yojson.Json_error e -> Error e)
  | Ok (s, body) ->
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
