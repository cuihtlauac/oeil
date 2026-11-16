module Facts = struct
  type t = {
    commit : string option;
    output : string option;
    inputs : string list;
    outcome : [ `Succeeded | `Failed of string | `Cancelled | `Unknown ];
    started : string option;
    tail : string list;
  }

  let of_log log =
    let output =
      match Job_log.build_digest log with
      | Some d -> Some d
      | None -> Job_log.output_digest log
    in
    let n = List.length log in
    {
      commit = Option.map snd (Job_log.commit log);
      output;
      inputs = Job_log.input_digests log;
      outcome = Job_log.outcome log;
      started = (match log with l :: _ -> Job_log.timestamp l | [] -> None);
      tail = List.filteri (fun i _ -> i >= n - 10) log;
    }
end

type target = { repo : string; branch : string; sha : string option }

type status =
  | Pending
  | Queued
  | Needs_confirmation
  | Running
  | Done
  | Unchanged
  | Failed of string
  | Blocked

type stage = {
  key : string;
  kind : Chain.kind;
  status : status;
  job : string option;
  since : string option;
}

type verdict = In_progress | Deployed | Failed_at of string

type t = {
  target : target;
  commit : string option;
  build_time : string option; (* earliest time key of our build jobs *)
  built : string list; (* digests of our builds *)
  published : string option; (* digest of the manifest *)
  stages : stage list;
  verdict : verdict;
}

type event =
  | Building of string
  | Superseded of string
  | Stage of { key : string; before : status; after : status; job : string option }
  | Finished of verdict

let create target =
  {
    target;
    commit = None;
    build_time = None;
    built = [];
    published = None;
    stages = [];
    verdict = In_progress;
  }

let target t = t.target
let stages t = t.stages
let verdict t = t.verdict
let commit t = t.commit
let digests t = (t.built, t.published)

let sha_matches a b =
  let a = String.lowercase_ascii a and b = String.lowercase_ascii b in
  let la = String.length a and lb = String.length b in
  min la lb >= 7 && if la <= lb then String.sub b 0 la = a else String.sub a 0 lb = b

(* "2026-11-16/082329-ocluster-build-13f132" -> "2026-11-16/082329" *)
let time_key job = if String.length job >= 17 then String.sub job 0 17 else job

let status_to_string = function
  | Pending -> "pending"
  | Queued -> "queued"
  | Needs_confirmation -> "needs-confirmation"
  | Running -> "running"
  | Done -> "done"
  | Unchanged -> "unchanged"
  | Failed m -> "failed: " ^ m
  | Blocked -> "blocked"

let is_final = function
  | Done | Unchanged | Failed _ -> true
  | Pending | Queued | Needs_confirmation | Running | Blocked -> false

let failure_message (n : Svg_graph.node) (f : Facts.t option) =
  match f with
  | Some { outcome = `Failed m; _ } -> m
  | Some { outcome = `Cancelled; _ } -> "cancelled"
  | _ -> ( match n.tooltip with Some m -> m | None -> "failed")

(* Status of a stage whose job is ours, from the node's colour. *)
let status_of_ours (n : Svg_graph.node) f =
  match Svg_graph.effective_state n with
  | State.Running -> Running
  | State.Ready -> Queued
  | State.Waiting_confirmation -> Needs_confirmation
  | State.Failed -> Failed (failure_message n f)
  | State.Blocked -> Blocked
  | State.Ok -> (
      match f with
      | Some { Facts.outcome = `Failed _ | `Cancelled; _ } -> Failed (failure_message n f)
      | _ -> Done)
  | State.Unknown _ -> Running

(* Status of a stage that has not run for our commit yet. *)
let status_of_other ~upstream_ready (n : Svg_graph.node) =
  match Svg_graph.effective_state n with
  | State.Blocked -> Blocked
  | State.Ready when upstream_ready -> Queued
  | State.Waiting_confirmation when upstream_ready -> Needs_confirmation
  | _ -> Pending

let previous t key = List.find_opt (fun (s : stage) -> s.key = key) t.stages

let step t ~(stages : Chain.stage list) ~facts ~includes =
  let events = ref [] in
  let emit e = events := e :: !events in
  let facts_of (s : Chain.stage) = Option.bind s.node.job facts in
  (* 1. Builds decide which commit we follow. *)
  let commit = ref t.commit in
  let build_time = ref t.build_time in
  let built = ref [] in
  let is_ours_commit c =
    match t.target.sha with
    | None -> true
    | Some target -> (
        sha_matches target c
        || (match t.commit with Some c' -> sha_matches c' c | None -> false)
        || match includes ~newer:c with Some b -> b | None -> false)
  in
  let keep_previous key =
    match previous t key with Some p -> p.status | None -> Pending
  in
  let stage_view (s : Chain.stage) status ~ours =
    let f = facts_of s in
    {
      key = s.key;
      kind = s.kind;
      status;
      job = (if ours then s.node.job else None);
      since = (if ours then Option.bind f (fun (f : Facts.t) -> f.started) else None);
    }
  in
  let build_views =
    List.filter_map
      (fun (s : Chain.stage) ->
        match s.kind with
        | Chain.Build _ -> (
            match facts_of s with
            | Some ({ commit = Some c; _ } as f) when is_ours_commit c ->
                (match !commit with
                | Some c' when sha_matches c' c -> ()
                | _ -> (
                    commit := Some c;
                    emit (Building c);
                    match t.target.sha with
                    | Some target when not (sha_matches target c) -> emit (Superseded c)
                    | _ -> ()));
                let tk = Option.map time_key s.node.job in
                (build_time :=
                   match (!build_time, tk) with
                   | Some a, Some b -> Some (min a b)
                   | None, b -> b
                   | a, None -> a);
                let st = status_of_ours s.node (Some f) in
                (match (st, f.output) with
                | Done, Some d -> built := d :: !built
                | _ -> ());
                Some (stage_view s st ~ours:true)
            | Some _ ->
                Some
                  (stage_view s
                     (status_of_other ~upstream_ready:false s.node)
                     ~ours:false)
            | None -> Some (stage_view s (keep_previous s.key) ~ours:false))
        | _ -> None)
      stages
  in
  let built = if !built = [] then t.built else List.rev !built in
  let builds_done = built <> [] in
  (* 2. Downstream stages follow digests. *)
  let older_than_build job =
    match (!build_time, job) with Some b, Some j -> time_key j < b | _ -> false
  in
  let published = ref t.published in
  let follow (s : Chain.stage) ~wanted ~upstream_ready =
    match facts_of s with
    | Some f when wanted <> [] && List.exists (fun d -> List.mem d f.inputs) wanted ->
        let st = status_of_ours s.node (Some f) in
        let st = if st = Done && older_than_build s.node.job then Unchanged else st in
        (st, f, true)
    | Some f -> (status_of_other ~upstream_ready s.node, f, false)
    | None ->
        ( keep_previous s.key,
          {
            Facts.commit = None;
            output = None;
            inputs = [];
            outcome = `Unknown;
            started = None;
            tail = [];
          },
          false )
  in
  let push_views =
    List.filter_map
      (fun (s : Chain.stage) ->
        match s.kind with
        | Chain.Push _ ->
            let st, f, ours = follow s ~wanted:built ~upstream_ready:builds_done in
            (match (st, f.output) with
            | (Done | Unchanged), Some d when ours -> published := Some d
            | _ -> ());
            Some (stage_view s st ~ours)
        | _ -> None)
      stages
  in
  let wanted = Option.to_list !published in
  let deploy_views =
    List.filter_map
      (fun (s : Chain.stage) ->
        match s.kind with
        | Chain.Pull _ | Chain.Service _ ->
            let st, _, ours = follow s ~wanted ~upstream_ready:(wanted <> []) in
            Some (stage_view s st ~ours)
        | _ -> None)
      stages
  in
  let notify_views =
    List.filter_map
      (fun (s : Chain.stage) ->
        match s.kind with
        | Chain.Notify ->
            let ours =
              match (!build_time, s.node.job) with
              | Some b, Some j -> time_key j >= b
              | _ -> false
            in
            let st = if ours then status_of_ours s.node (facts_of s) else Pending in
            Some (stage_view s st ~ours)
        | _ -> None)
      stages
  in
  let views = build_views @ push_views @ deploy_views @ notify_views in
  (* Keep chain order. *)
  let views =
    List.filter_map
      (fun (s : Chain.stage) -> List.find_opt (fun (v : stage) -> v.key = s.key) views)
      stages
  in
  List.iter
    (fun (v : stage) ->
      let before = keep_previous v.key in
      if before <> v.status || previous t v.key = None then
        emit (Stage { key = v.key; before; after = v.status; job = v.job }))
    views;
  let verdict =
    match
      List.find_opt
        (fun (v : stage) -> match v.status with Failed _ -> true | _ -> false)
        views
    with
    | Some v -> Failed_at v.key
    | None ->
        let pick k = List.filter (fun (v : stage) -> k v.kind) views in
        let goal =
          match pick (function Chain.Service _ -> true | _ -> false) with
          | [] -> (
              match pick (function Chain.Push _ -> true | _ -> false) with
              | [] -> pick (function Chain.Build _ -> true | _ -> false)
              | l -> l)
          | l -> l
        in
        if
          goal <> []
          && List.for_all
               (fun (v : stage) -> v.status = Done || v.status = Unchanged)
               goal
        then Deployed
        else In_progress
  in
  if verdict <> t.verdict && verdict <> In_progress then emit (Finished verdict);
  ( {
      t with
      commit = !commit;
      build_time = !build_time;
      built;
      published = !published;
      stages = views;
      verdict;
    },
    List.rev !events )

let jobs_to_fetch (stages : Chain.stage list) _t =
  List.filter_map (fun (s : Chain.stage) -> s.node.job) stages |> List.sort_uniq compare
