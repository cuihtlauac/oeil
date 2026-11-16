(* The state of every merged PR of a repository's deployed branches. *)
open Oeil

type branch = {
  branch : string;
  stages : Chain.stage list;
  tracker : Tracker.t option;  (** the branch's latest build and its deploy *)
  deploys : Repo_state.deploy list;  (** recent deploys, newest first *)
  live : string option;  (** the live commit *)
  live_since : string option;  (** the deploy (service job id) that made it live *)
  head : string option;
  entries : Repo_state.entry list;
  errors : string list;
}

type t = {
  repo : string;
  branches : branch list;
  feeds : Ci.feed list;  (** workflows that run on a schedule or open PRs *)
  feed_errors : string list;
  at : float;
}

type ctx = {
  jobs : Jobs.t;
  github : Github.t;
  compares : (string * string, Repo_state.commit list) Hashtbl.t;
  mutable prs : (string * (float * Repo_state.pr list)) list;  (** by branch *)
  owners : (string, Repo_state.pr option) Hashtbl.t;  (** commit -> its PR *)
}

let create ~jobs ~github =
  { jobs; github; compares = Hashtbl.create 16; prs = []; owners = Hashtbl.create 16 }

(* The PR a commit without one of its own came with (a rebase merge brings
   several commits; only the last is the PR's merge commit). *)
let owner ctx ~repo ~branch sha =
  match Hashtbl.find_opt ctx.owners sha with
  | Some p -> p
  | None -> (
      match Github.pulls_of_commit ctx.github ~repo ~sha ~base:branch with
      | Ok l ->
          let p = match l with p :: _ -> Some p | [] -> None in
          Hashtbl.replace ctx.owners sha p;
          p
      | Error _ -> None)

(* First-parent commits from [base] (excluded) to [head], newest first.
   Compares between two shas never change: cache them. *)
let between ctx ~repo ~base ~head =
  if Tracker.sha_matches base head then Ok []
  else
    match Hashtbl.find_opt ctx.compares (base, head) with
    | Some l -> Ok l
    | None ->
        Result.map
          (fun (_status, commits) ->
            let l = Repo_state.first_parent ~head commits in
            Hashtbl.replace ctx.compares (base, head) l;
            l)
          (Github.compare_commits ctx.github ~repo ~base ~head)

let prs ctx ~repo ~branch =
  match List.assoc_opt branch ctx.prs with
  | Some (at, l) when Unix.gettimeofday () -. at < 60. -> Ok l
  | _ ->
      Result.map
        (fun l ->
          ctx.prs <-
            (branch, (Unix.gettimeofday (), l)) :: List.remove_assoc branch ctx.prs;
          l)
        (Github.merged_pulls ctx.github ~repo ~base:branch ())

(* The branch's latest build, followed like [oeil watch owner/repo:branch]. *)
let latest_build ctx ~repo ~branch (stages : Chain.stage list) =
  let facts job =
    let active =
      List.exists
        (fun (s : Chain.stage) ->
          s.node.job = Some job && State.is_active (Svg_graph.effective_state s.node))
        stages
    in
    if active then
      Result.to_option (Result.map Tracker.Facts.of_log (Jobs.head ctx.jobs job))
    else Result.to_option (Jobs.facts ctx.jobs job)
  in
  (* Only finished jobs: a running job's page only ends with the job. *)
  Jobs.prefetch_facts ctx.jobs
    (List.filter_map
       (fun (s : Chain.stage) ->
         if State.is_active (Svg_graph.effective_state s.node) then None else s.node.job)
       stages);
  let t = Tracker.create { repo; branch; sha = None } in
  fst (Tracker.step t ~stages ~facts ~includes:(fun ~newer:_ -> Some true))

(* Where a deploy in progress is: its first stage that is not final. *)
let furthest (t : Tracker.t) =
  match
    List.find_opt
      (fun (s : Tracker.stage) -> not (Tracker.is_final s.status))
      (Tracker.stages t)
  with
  | Some s -> Chain.kind_name s.kind ^ " " ^ Tracker.status_to_string s.status
  | None -> "finishing"

type deploy_state = {
  tracker : Tracker.t;  (** the branch's latest build *)
  deploys : Repo_state.deploy list;  (** recent deploys, newest first *)
  newest : Repo_state.deploy option;  (** the newest deploy whose commit is known *)
  live : string option;
  live_since : string option;
}

(* What a deployed branch runs: its latest build, its recent deploys, and
   its live commit. *)
let deploy_state ctx ~repo ~branch ~since stages =
  let tracker = latest_build ctx ~repo ~branch stages in
  let deploys = Deploys.history ctx.jobs ~repo ~branch ~stages ~since in
  let newest =
    List.find_opt (fun (d : Repo_state.deploy) -> Option.is_some d.commit) deploys
  in
  (* The newest deploy's commit, or the latest build's when its image is the
     one already deployed. *)
  let live, live_since =
    match (Tracker.verdict tracker, Tracker.commit tracker, newest) with
    | Tracker.Deployed, Some b, Some d -> (Some b, Some d.at)
    | Tracker.Deployed, Some b, None -> (Some b, None)
    | _, _, Some d -> (d.commit, Some d.at)
    | _, _, None -> (None, None)
  in
  { tracker; deploys; newest; live; live_since }

let branch_summary ctx ~repo ~graph ~since branch =
  let errors = ref [] in
  let ok = function
    | Ok x -> Some x
    | Error e ->
        errors := e :: !errors;
        None
  in
  match Chain.stages graph ~repo ~branch with
  | Error e ->
      {
        branch;
        stages = [];
        tracker = None;
        deploys = [];
        live = None;
        live_since = None;
        head = None;
        entries = [];
        errors = [ e ];
      }
  | Ok stages ->
      let { tracker; deploys; newest; live; live_since } =
        deploy_state ctx ~repo ~branch ~since stages
      in
      let head = ok (Github.head ctx.github ~repo ~branch) in
      let prs = Option.value ~default:[] (ok (prs ctx ~repo ~branch)) in
      let after_live =
        match (live, head) with
        | Some l, Some h ->
            Option.value ~default:[] (ok (between ctx ~repo ~base:l ~head:h))
        | _ -> []
      in
      let inflight =
        match (Tracker.commit tracker, live) with
        | Some b, Some l when not (Tracker.sha_matches b l) -> (
            match Tracker.verdict tracker with
            | Tracker.Failed_at k -> Some (b, `Failed k)
            | Tracker.In_progress -> Some (b, `Running (furthest tracker))
            | Tracker.Deployed -> None)
        | _ -> None
      in
      (* Pairs of consecutive deploys with known, different commits. *)
      let rec pairs (l : Repo_state.deploy list) =
        match l with
        | d1 :: (d0 :: _ as rest) -> (
            match (d1.commit, d0.commit) with
            | Some c1, Some c0 when not (Tracker.sha_matches c0 c1) ->
                (d1, c0, c1) :: pairs rest
            | _ -> pairs rest)
        | _ -> []
      in
      let recent_deploys =
        List.filter
          (fun (d : Repo_state.deploy) ->
            match Jobs.seconds_of_key (Jobs.time_key d.at) with
            | Some t -> t >= since
            | None -> false)
          deploys
      in
      let deploy_pairs =
        pairs deploys |> List.filter (fun (d, _, _) -> List.memq d recent_deploys)
      in
      (* A build whose image was already live brings its commits live too. *)
      let unchanged_pair =
        match (live, newest) with
        | Some l, Some (d : Repo_state.deploy) -> (
            match d.commit with
            | Some c when not (Tracker.sha_matches l c) -> [ (d, c, l) ]
            | _ -> [])
        | _ -> []
      in
      let went_live =
        Eio.Fiber.List.filter_map ~max_fibers:4
          (fun (d, base, head) ->
            Option.map (fun l -> (d, l)) (ok (between ctx ~repo ~base ~head)))
          (unchanged_pair @ deploy_pairs)
      in
      let entries =
        Repo_state.classify ~prs ~after_live ~inflight ~went_live
        |> Eio.Fiber.List.map ~max_fibers:4 (fun (e : Repo_state.entry) ->
            match e.pr with
            | Some _ -> e
            | None -> { e with pr = owner ctx ~repo ~branch e.commit.sha })
        |> Repo_state.by_pr
      in
      {
        branch;
        stages;
        tracker = Some tracker;
        deploys;
        live;
        live_since;
        head;
        entries;
        errors = List.rev !errors;
      }

let snapshot ctx ~repo ?(recent = 86400.) () =
  match Instance.pipeline (Jobs.instance ctx.jobs) ~query:("repo=" ^ repo) () with
  | Error e -> Error e
  | Ok graph ->
      let since = Unix.gettimeofday () -. recent in
      let branches, (feeds, feed_errors) =
        Eio.Fiber.pair
          (fun () ->
            Chain.branches graph ~repo
            |> Eio.Fiber.List.map (branch_summary ctx ~repo ~graph ~since)
            |> List.rev (* main first: the graph lists it last *))
          (fun () ->
            match Feeds.fetch ctx.github ~repo ~days:14 with
            | Ok r -> r
            | Error e -> ([], [ e ]))
      in
      Jobs.save ctx.jobs;
      Ok { repo; branches; feeds; feed_errors; at = Unix.gettimeofday () }

let jobs ctx = ctx.jobs
