(* JSON views of oeil's data, shared by the MCP tools. *)
open Oeil

let opt f = function Some x -> f x | None -> `Null
let str s = `String s

let status (s : Tracker.status) =
  match s with
  | Failed m -> `Assoc [ ("status", `String "failed"); ("error", `String m) ]
  | s -> `Assoc [ ("status", `String (Tracker.status_to_string s)) ]

let stage ~job_url (st : Tracker.stage) =
  let fields = match status st.status with `Assoc l -> l | _ -> [] in
  `Assoc
    ([ ("key", `String st.key); ("stage", `String (Chain.kind_label st.kind)) ]
    @ fields
    @ [
        ("job", opt str st.job);
        ("url", opt (fun j -> `String (job_url j)) st.job);
        ("started", opt str st.since);
      ])

let verdict = function
  | Tracker.In_progress -> `Assoc [ ("verdict", `String "in_progress") ]
  | Deployed -> `Assoc [ ("verdict", `String "deployed") ]
  | Failed_at k -> `Assoc [ ("verdict", `String "failed"); ("failed_stage", `String k) ]

let event (e : Tracker.event) =
  match e with
  | Building c -> `Assoc [ ("event", `String "building"); ("commit", `String c) ]
  | Superseded c -> `Assoc [ ("event", `String "superseded"); ("commit", `String c) ]
  | Stage { key; before; after; job } ->
      `Assoc
        [
          ("event", `String "stage");
          ("key", `String key);
          ("from", `String (Tracker.status_to_string before));
          ("to", `String (Tracker.status_to_string after));
          ("job", opt str job);
        ]
  | Finished v -> (
      match verdict v with
      | `Assoc l -> `Assoc (("event", `String "finished") :: l)
      | j -> j)

let tracker ~job_url ~title t =
  let target = Tracker.target t in
  let built, published = Tracker.digests t in
  let v = match verdict (Tracker.verdict t) with `Assoc l -> l | _ -> [] in
  `Assoc
    ([
       ("title", `String title);
       ( "target",
         `Assoc
           [
             ("repo", `String target.repo);
             ("branch", `String target.branch);
             ("sha", opt str target.sha);
           ] );
       ("commit", opt str (Tracker.commit t));
     ]
    @ v
    @ [
        ("stages", `List (List.map (stage ~job_url) (Tracker.stages t)));
        ( "digests",
          `Assoc
            [ ("built", `List (List.map str built)); ("published", opt str published) ] );
      ])

let node (n : Svg_graph.node) =
  `Assoc
    [
      ("label", `String (Svg_graph.label n));
      ("state", `String (State.to_string (Svg_graph.effective_state n)));
      ("job", opt str n.job);
      ("expand", opt str n.expand);
      ("error", opt str n.tooltip);
    ]

let repo_state (s : Repo_state.state) =
  match s with
  | Waiting -> [ ("state", `String "waiting") ]
  | Deploying d -> [ ("state", `String "deploying"); ("stage", `String d) ]
  | Failed k -> [ ("state", `String "failed"); ("failed_stage", `String k) ]
  | Live at -> [ ("state", `String "live"); ("live_since", opt str at) ]

let entry (e : Repo_state.entry) =
  `Assoc
    ((match e.pr with
       | Some p ->
           [
             ("pr", `Int p.number);
             ("title", `String p.title);
             ("author", `String p.author);
             ("merged_at", `String p.merged_at);
           ]
       | None -> [ ("pr", `Null); ("title", `String e.commit.message) ])
    @ [ ("commit", `String e.commit.sha) ]
    @ repo_state e.state)

let iso t =
  match Ptime.of_float_s t with
  | Some p -> `String (Ptime.to_rfc3339 ~tz_offset_s:0 p)
  | None -> `Null

let feed ~now (f : Ci.feed) =
  `Assoc
    [
      ("name", `String f.name);
      ("workflow", `String f.path);
      ("schedules", `List (List.map (fun (c : Cron.t) -> `String c.text) f.schedules));
      ( "next_run",
        match
          List.filter_map (fun c -> Cron.next c ~now) f.schedules |> List.sort compare
        with
        | t :: _ -> iso t
        | [] -> `Null );
      ("bot_branch", opt str f.branch);
      ("latest_pr", match Ci.last_pr f with Some n -> `Int n | None -> `Null);
      ( "runs",
        `List
          (List.map
             (fun ((r : Ci.run), o) ->
               `Assoc
                 [
                   ("started", iso r.started);
                   ("event", `String r.event);
                   ("outcome", `String (Ci.outcome_to_string o));
                   ("url", `String r.url);
                 ])
             f.runs) );
    ]

let timeline ~repo ~number ~title ~state (steps : Lifecycle.step list) =
  let status = function
    | Lifecycle.Done -> "done"
    | Running -> "running"
    | Failed -> "failed"
    | Pending -> "pending"
    | Waiting -> "waiting"
  in
  `Assoc
    ([
       ("repo", `String repo);
       ("pr", `Int number);
       ("title", `String title);
       ("state", `String state);
       ( "steps",
         `List
           (List.map
              (fun (s : Lifecycle.step) ->
                `Assoc
                  [
                    ("step", `String s.label);
                    ("part_of_previous", `Bool (s.depth > 0));
                    ("status", `String (status s.status));
                    ("at", opt iso s.time);
                    ("seconds", opt (fun d -> `Float (Float.round d)) s.duration);
                    ("detail", `String s.detail);
                    ( "log",
                      match s.log with
                      | Some (Lifecycle.Deployer_job j) ->
                          `Assoc [ ("deployer_job", `String j) ]
                      | Some (Actions_job j) -> `Assoc [ ("actions_job", `Int j) ]
                      | Some (Ocurrent_job { base; job }) ->
                          `Assoc [ ("ocurrent", `String base); ("job", `String job) ]
                      | None -> `Null );
                  ])
              steps) );
     ]
    @
    match Lifecycle.summary steps with
    | Some (total, waiting) ->
        [
          ("total_seconds", `Float (Float.round total));
          ("waiting_for_review_seconds", `Float (Float.round waiting));
        ]
    | None -> [])

let repo ~job_url (s : Oeil_net.Repo_monitor.t) =
  let now = Unix.gettimeofday () in
  `Assoc
    [
      ("repo", `String s.repo);
      ("feeds", `List (List.map (feed ~now) s.feeds));
      ( "branches",
        `List
          (List.map
             (fun (b : Oeil_net.Repo_monitor.branch) ->
               `Assoc
                 [
                   ("branch", `String b.branch);
                   ("live_commit", opt str b.live);
                   ("live_since", opt str b.live_since);
                   ("head", opt str b.head);
                   ( "latest_build",
                     match b.tracker with
                     | None -> `Null
                     | Some t -> (
                         match verdict (Tracker.verdict t) with
                         | `Assoc v ->
                             `Assoc
                               ((("commit", opt str (Tracker.commit t)) :: v)
                               @ [
                                   ( "stages",
                                     `List (List.map (stage ~job_url) (Tracker.stages t))
                                   );
                                 ])
                         | j -> j) );
                   ("prs", `List (List.map entry b.entries));
                   ("errors", `List (List.map str b.errors));
                 ])
             s.branches) );
    ]
