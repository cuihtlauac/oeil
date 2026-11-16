type t =
  | Pr of { repo : string; number : int }
  | Commit of { repo : string; sha : string }
  | Branch of { repo : string; branch : string }

let valid_repo r =
  match String.split_on_char '/' r with [ o; n ] -> o <> "" && n <> "" | _ -> false

let parse s =
  let s = String.trim s in
  let split c =
    match String.index_opt s c with
    | Some i -> Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
    | None -> None
  in
  let check repo k =
    if valid_repo repo then k () else Error ("not an owner/repo: " ^ repo)
  in
  match (split '#', split '@', split ':') with
  | Some (repo, n), _, _ -> (
      check repo @@ fun () ->
      match int_of_string_opt n with
      | Some number when number > 0 -> Ok (Pr { repo; number })
      | _ -> Error ("not a pull request number: " ^ n))
  | None, Some (repo, sha), _ ->
      check repo @@ fun () ->
      if String.length sha >= 7 && Job_log.is_hex sha then
        Ok (Commit { repo; sha = String.lowercase_ascii sha })
      else Error ("not a commit SHA (7+ hex digits): " ^ sha)
  | None, None, Some (repo, branch) ->
      check repo @@ fun () ->
      if branch <> "" then Ok (Branch { repo; branch }) else Error "empty branch name"
  | None, None, None ->
      if valid_repo s then Ok (Branch { repo = s; branch = "main" })
      else
        Error
          (Printf.sprintf
             "cannot parse target %S: expected owner/repo#PR, owner/repo@SHA or \
              owner/repo:branch"
             s)

let repo = function Pr { repo; _ } | Commit { repo; _ } | Branch { repo; _ } -> repo

let to_string = function
  | Pr { repo; number } -> Printf.sprintf "%s#%d" repo number
  | Commit { repo; sha } -> Printf.sprintf "%s@%s" repo sha
  | Branch { repo; branch } -> Printf.sprintf "%s:%s" repo branch
