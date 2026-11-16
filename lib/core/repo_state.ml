(* Where each merged pull request of a deployed branch stands. Pure.

   What is live on a branch is one commit, the one the latest successful
   deploy built. A PR's state follows from where its merge commit sits on the
   branch's first-parent history, not from its age or merge date:

     … live commit │ in-flight build │ merged since │ head
       live          deploying/failed  waiting
*)

type commit = {
  sha : string;
  parents : string list;
  message : string;  (** first line *)
  date : string;
}

type pr = {
  number : int;
  title : string;
  merge_commit : string;
  merged_at : string;
  author : string;
}

type deploy = {
  at : string;  (** job id of the service update, e.g. "2026-11-16/094513-…" *)
  digest : string;  (** image deployed *)
  commit : string option;  (** the commit it was built from, if found *)
  push_job : string option;  (** the push-manifest job that published [digest] *)
  build_job : string option;  (** the build of [commit] *)
}

type state =
  | Waiting  (** merged, not in a build yet *)
  | Deploying of string  (** in the build being deployed; e.g. "build running" *)
  | Failed of string  (** in a build whose deploy failed: stage key *)
  | Live of string option  (** live; when it went live (deploy job id), if known *)

type entry = {
  pr : pr option;  (** [None] for a commit pushed without a PR *)
  commit : commit;
  state : state;
}

(* The first-parent chain from [head] back to (excluding) [base], newest
   first, from the commits a compare returned. Commits brought in by a merge
   commit's other parents are left out: they are not changes to the branch. *)
let first_parent ~head commits =
  let by_sha = Hashtbl.create 16 in
  List.iter (fun c -> Hashtbl.replace by_sha c.sha c) commits;
  let rec go sha acc =
    match Hashtbl.find_opt by_sha sha with
    | None -> List.rev acc
    | Some c -> (
        match c.parents with p :: _ -> go p (c :: acc) | [] -> List.rev (c :: acc))
  in
  go head []

let entries_of ~prs ~state commits =
  List.map
    (fun c ->
      let pr = List.find_opt (fun p -> Tracker.sha_matches p.merge_commit c.sha) prs in
      { pr; commit = c; state })
    commits

let mem_sha sha l = List.exists (fun c -> Tracker.sha_matches c.sha sha) l

(* [classify] builds the summary of a branch:
   - [after_live]: first-parent commits from the live commit (excluded) to
     the branch head, newest first;
   - [inflight]: the commit of the build being deployed, if it is not the
     live one, and how that deploy goes;
   - [went_live]: for recent deploys, newest first, the deploy and the
     first-parent commits it brought live. *)
let classify ~prs ~after_live ~inflight ~went_live =
  let deploying, waiting =
    match inflight with
    | None -> ([], after_live)
    | Some (b, _) ->
        (* Commits at or below [b] are in the build; above it, waiting. *)
        let rec split above = function
          | [] -> (List.rev above, [])
          | c :: rest when Tracker.sha_matches c.sha b -> (List.rev above, c :: rest)
          | c :: rest -> split (c :: above) rest
        in
        let above, from_b = split [] after_live in
        if from_b = [] then ([], after_live) else (from_b, above)
  in
  let inflight_state =
    match inflight with
    | Some (_, `Failed k) -> Failed k
    | Some (_, `Running s) -> Deploying s
    | None -> Waiting
  in
  let live =
    List.concat_map
      (fun (d, commits) ->
        entries_of ~prs ~state:(Live (Some d.at)) commits
        |> List.filter (fun e -> not (mem_sha e.commit.sha after_live)))
      went_live
  in
  entries_of ~prs ~state:Waiting waiting
  @ entries_of ~prs ~state:inflight_state deploying
  @ live

(* One entry per PR: the first, newest, of its commits' entries. Commits
   without a PR stay as they are. *)
let by_pr entries =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun e ->
      match e.pr with
      | None -> true
      | Some p ->
          if Hashtbl.mem seen p.number then false
          else (
            Hashtbl.replace seen p.number ();
            true))
    entries

let state_to_string = function
  | Waiting -> "waiting"
  | Deploying s -> "deploying: " ^ s
  | Failed k -> "failed at " ^ k
  | Live _ -> "live"
