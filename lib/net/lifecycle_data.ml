(* The GitHub side of a change's life: the run that opened its PR, the PR,
   its checks, and its merge. *)
open Oeil
module L = Lifecycle

let jobs_of_run gh ~repo (r : Ci.run) =
  match Actions.jobs gh ~repo ~run:r.id with
  | Ok jobs ->
      List.map
        (fun (j : Ci.job) ->
          L.step ?time:j.job_started ~depth:1
            ~key:(Printf.sprintf "job:%d" j.job_id)
            ~status:(L.status_of_conclusion j.job_conclusion)
            ?duration:(L.span j.job_started j.job_finished)
            ~detail:(match j.failed_step with Some s -> "failed at: " ^ s | None -> "")
            ~log:(L.Actions_job j.job_id) j.name)
        jobs
  | Error _ -> []

let run_step ~feed (r : Ci.run) outcome =
  L.step ~time:r.started
    ~key:(Printf.sprintf "run:%d" r.id)
    ~status:
      (match outcome with
      | Ci.In_progress -> L.Running
      | Failed _ -> L.Failed
      | _ -> L.Done)
    ?duration:(L.span (Some r.started) r.finished)
    ~detail:(r.event ^ " · " ^ Ci.outcome_to_string outcome)
    (feed ^ " run")

(* A run of a feed, with its jobs. *)
let of_run gh ~repo ~feed ((r : Ci.run), outcome) =
  run_step ~feed r outcome :: jobs_of_run gh ~repo r

let pr gh ~repo ~(feeds : Ci.feed list) ~number =
  match Actions.pull gh ~repo ~number with
  | Error e -> Error e
  | Ok p ->
      let now = Unix.gettimeofday () in
      (* The bot runs that opened or updated it. *)
      let trigger =
        match List.find_opt (fun (f : Ci.feed) -> f.branch = Some p.p_head_ref) feeds with
        | None -> []
        | Some f ->
            f.runs
            |> List.filter (fun (_, o) ->
                match o with Ci.Opened n | Updated n -> n = number | _ -> false)
            |> List.rev
            |> List.concat_map (of_run gh ~repo ~feed:f.name)
      in
      let opened =
        L.step ~time:p.p_created ~key:"opened" ~status:L.Done
          ~detail:("by " ^ p.p_user ^ " from " ^ p.p_head_ref)
          (Printf.sprintf "PR #%d opened" number)
      in
      let checks =
        match Actions.checks gh ~repo ~sha:p.p_head_sha with Ok c -> c | Error _ -> []
      in
      let check_steps =
        List.map
          (fun (c : Actions.check) ->
            L.step ?time:c.check_started ~depth:1 ~key:("check:" ^ c.check_name)
              ~status:
                (if c.check_status <> "completed" then L.Running
                 else L.status_of_conclusion c.check_conclusion)
              ?duration:(L.span c.check_started c.check_finished)
              ~detail:(if c.app = "github-actions" then "" else c.app)
              ?log:(Option.map (fun j -> L.Actions_job j) c.actions_job)
              c.check_name)
          checks
        |> List.sort (fun (a : L.step) b -> compare a.time b.time)
      in
      let checks_step =
        match check_steps with
        | [] -> []
        | _ ->
            let starts = List.filter_map (fun (s : L.step) -> s.time) check_steps in
            let ends =
              List.filter_map (fun (c : Actions.check) -> c.check_finished) checks
            in
            let first = List.fold_left Float.min Float.infinity starts in
            let last = List.fold_left Float.max Float.neg_infinity ends in
            let status =
              if List.exists (fun (s : L.step) -> s.status = L.Failed) check_steps then
                L.Failed
              else if List.exists (fun (s : L.step) -> s.status = L.Running) check_steps
              then L.Running
              else L.Done
            in
            let failed =
              List.filter (fun (s : L.step) -> s.status = L.Failed) check_steps
            in
            L.step
              ?time:(if starts = [] then None else Some first)
              ~key:"checks" ~status
              ?duration:(if ends = [] || starts = [] then None else Some (last -. first))
              ~detail:
                (match failed with
                | [] -> Printf.sprintf "%d checks" (List.length check_steps)
                | l ->
                    "failed: "
                    ^ String.concat ", " (List.map (fun (s : L.step) -> s.label) l))
              "CI checks"
            :: check_steps
      in
      let checks_end =
        List.fold_left
          (fun acc (c : Actions.check) ->
            match c.check_finished with Some t -> Float.max acc t | None -> acc)
          p.p_created checks
      in
      let merge =
        match p.p_merged with
        | Some m ->
            let since = Float.min checks_end m in
            [
              L.step ~time:since ~key:"merge:wait" ~status:L.Done ~duration:(m -. since)
                ~detail:
                  (match p.p_merged_by with Some u -> "merged by " ^ u | None -> "")
                "review and merge";
              L.step ~time:m ~key:"merged" ~status:L.Done
                ~detail:
                  (match p.p_merge_commit with
                  | Some c -> p.p_base ^ " @ " ^ String.sub c 0 (min 9 (String.length c))
                  | None -> p.p_base)
                "merged";
            ]
        | None when p.p_state = "closed" ->
            [ L.step ~key:"closed" ~status:L.Failed "closed without merging" ]
        | None ->
            [
              L.step ~time:checks_end ~key:"merge:wait" ~status:L.Waiting
                ~duration:(now -. checks_end) "waiting for review and merge";
            ]
      in
      Ok (p, trigger @ [ opened ] @ checks_step @ merge)
