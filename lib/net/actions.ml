(* GitHub Actions: workflows, runs, jobs, checks and logs. *)
open Oeil
open Yojson.Safe.Util

let ( let* ) = Result.bind

let parse f j =
  try Ok (f j) with Type_error (m, _) -> Error ("unexpected GitHub reply: " ^ m)

let time j k = Option.bind (j |> member k |> to_string_option) Ci.time_of_iso

type workflow = { wf_id : int; wf_name : string; wf_path : string; wf_state : string }

let workflows gh ~repo =
  let* j =
    Github.get_json gh (Printf.sprintf "/repos/%s/actions/workflows?per_page=100" repo)
  in
  parse
    (fun j ->
      j |> member "workflows" |> to_list
      |> List.map (fun w ->
          {
            wf_id = w |> member "id" |> to_int;
            wf_name = w |> member "name" |> to_string;
            wf_path = w |> member "path" |> to_string;
            wf_state = w |> member "state" |> to_string;
          }))
    j

(* The text of a file at the default branch. *)
let file gh ~repo ~path =
  let* j = Github.get_json gh (Printf.sprintf "/repos/%s/contents/%s" repo path) in
  let* content = parse (fun j -> j |> member "content" |> to_string) j in
  let b64 = String.concat "" (String.split_on_char '\n' content) in
  match Base64.decode b64 with Ok s -> Ok s | Error (`Msg m) -> Error m

let run_of j : Ci.run =
  {
    id = j |> member "id" |> to_int;
    event = j |> member "event" |> to_string;
    status = j |> member "status" |> to_string;
    conclusion = j |> member "conclusion" |> to_string_option;
    started =
      (match time j "run_started_at" with
      | Some t -> t
      | None -> Option.value (time j "created_at") ~default:0.);
    finished =
      (if j |> member "status" |> to_string = "completed" then time j "updated_at"
       else None);
    head_sha = j |> member "head_sha" |> to_string;
    url = j |> member "html_url" |> to_string;
  }

(* Runs of a workflow created on or after [since] ("2026-11-02"), newest first. *)
let runs gh ~repo ~workflow ~since =
  let* j =
    Github.get_json gh
      (Printf.sprintf
         "/repos/%s/actions/workflows/%d/runs?per_page=100&created=%%3E%%3D%s" repo
         workflow since)
  in
  parse (fun j -> j |> member "workflow_runs" |> to_list |> List.map run_of) j

let jobs gh ~repo ~run =
  let* j =
    Github.get_json gh (Printf.sprintf "/repos/%s/actions/runs/%d/jobs" repo run)
  in
  parse
    (fun j ->
      j |> member "jobs" |> to_list
      |> List.map (fun jb ->
          let steps = try jb |> member "steps" |> to_list with _ -> [] in
          {
            Ci.job_id = jb |> member "id" |> to_int;
            name = jb |> member "name" |> to_string;
            job_conclusion = jb |> member "conclusion" |> to_string_option;
            job_started = time jb "started_at";
            job_finished = time jb "completed_at";
            failed_step =
              List.find_map
                (fun s ->
                  match s |> member "conclusion" |> to_string_option with
                  | Some "failure" -> Some (s |> member "name" |> to_string)
                  | _ -> None)
                steps;
          }))
    j

type check = {
  check_name : string;
  app : string;  (** "github-actions", "deploy-ci-ocaml-org", … *)
  check_status : string;
  check_conclusion : string option;
  check_started : float option;
  check_finished : float option;
  actions_job : int option;  (** the Actions job behind it, for its log *)
  details : string option;
}

let checks gh ~repo ~sha =
  let* j =
    Github.get_json gh
      (Printf.sprintf "/repos/%s/commits/%s/check-runs?per_page=100" repo sha)
  in
  parse
    (fun j ->
      j |> member "check_runs" |> to_list
      |> List.map (fun c ->
          let app = try c |> member "app" |> member "slug" |> to_string with _ -> "" in
          {
            check_name = c |> member "name" |> to_string;
            app;
            check_status = c |> member "status" |> to_string;
            check_conclusion = c |> member "conclusion" |> to_string_option;
            check_started = time c "started_at";
            check_finished = time c "completed_at";
            actions_job =
              (if app = "github-actions" then Some (c |> member "id" |> to_int) else None);
            details = c |> member "details_url" |> to_string_option;
          }))
    j

(* PRs from [branch] of [repo], newest first, with their commits' dates. *)
let bot_prs gh ~repo ~branch ~since =
  let owner = match String.split_on_char '/' repo with o :: _ -> o | [] -> repo in
  let* j =
    Github.get_json gh
      (Printf.sprintf
         "/repos/%s/pulls?state=all&head=%s:%s&sort=created&direction=desc&per_page=30"
         repo owner branch)
  in
  let* prs =
    parse
      (fun j ->
        j |> to_list
        |> List.map (fun p ->
            ( p |> member "number" |> to_int,
              Option.value (time p "created_at") ~default:0.,
              time p "merged_at",
              p |> member "state" |> to_string = "closed" && time p "merged_at" = None,
              Option.value (time p "updated_at") ~default:0. )))
      j
  in
  (* Commits only for PRs active since [since]: older ones did not change. *)
  Ok
    (Eio.Fiber.List.map ~max_fibers:4
       (fun (number, created, merged, closed, updated) ->
         let commit_dates =
           if updated < since then []
           else
             match
               Github.get_json gh
                 (Printf.sprintf "/repos/%s/pulls/%d/commits?per_page=100" repo number)
             with
             | Ok j -> (
                 try
                   j |> to_list
                   |> List.filter_map (fun c ->
                       Option.bind
                         (c |> member "commit" |> member "committer" |> member "date"
                        |> to_string_option)
                         Ci.time_of_iso)
                 with _ -> [])
             | Error _ -> []
         in
         { Ci.number; created; merged; closed; commit_dates })
       prs)

type pull = {
  p_number : int;
  p_title : string;
  p_user : string;
  p_head_ref : string;
  p_head_sha : string;
  p_created : float;
  p_merged : float option;
  p_merged_by : string option;
  p_merge_commit : string option;
  p_base : string;
  p_state : string;
}

let pull gh ~repo ~number =
  let* j = Github.get_json gh (Printf.sprintf "/repos/%s/pulls/%d" repo number) in
  parse
    (fun p ->
      {
        p_number = number;
        p_title = p |> member "title" |> to_string;
        p_user = (try p |> member "user" |> member "login" |> to_string with _ -> "");
        p_head_ref = p |> member "head" |> member "ref" |> to_string;
        p_head_sha = p |> member "head" |> member "sha" |> to_string;
        p_created = Option.value (time p "created_at") ~default:0.;
        p_merged = time p "merged_at";
        p_merged_by =
          (try Some (p |> member "merged_by" |> member "login" |> to_string)
           with _ -> None);
        p_merge_commit = p |> member "merge_commit_sha" |> to_string_option;
        p_base = p |> member "base" |> member "ref" |> to_string;
        p_state = p |> member "state" |> to_string;
      })
    j

(* A job's log. GitHub answers with a redirect to a signed URL. *)
let job_log gh ~repo ~job =
  let headers =
    ("Accept", "application/vnd.github+json")
    ::
    (match Github.token gh with
    | Some t -> [ ("Authorization", "Bearer " ^ t) ]
    | None -> [])
  in
  let uri =
    Uri.of_string
      (Printf.sprintf "https://api.github.com/repos/%s/actions/jobs/%d/logs" repo job)
  in
  let* status, h, body = Web.get_full ~headers (Github.web gh) uri in
  let* text =
    match status with
    | 200 -> Ok body
    | 302 | 301 | 307 -> (
        match Http.Header.get h "location" with
        | None -> Error "job log: redirect without a location"
        | Some loc -> (
            match Web.get (Github.web gh) (Uri.of_string loc) with
            | Ok (200, b) -> Ok b
            | Ok (s, _) -> Error (Printf.sprintf "job log: HTTP %d" s)
            | Error _ as e -> e))
    | 404 | 410 -> Error "job log: not available (expired, or no access)"
    | s -> Error (Printf.sprintf "job log: HTTP %d" s)
  in
  (* "2026-11-15T05:37:13.0863029Z Download…" -> "06:37:13 Download…", in
     local time like the rest of oeil. *)
  let short l =
    if String.length l > 28 && l.[10] = 'T' && l.[27] = 'Z' then
      match Ci.time_of_iso (String.sub l 0 19) with
      | Some t ->
          let tm = Unix.localtime t in
          Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec
          ^ String.sub l 28 (String.length l - 28)
      | None -> l
    else l
  in
  Ok (String.split_on_char '\n' text |> List.map short)
