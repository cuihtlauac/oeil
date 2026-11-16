open Oeil
open Oeil_net

let src = Logs.Src.create "oeil.mcp" ~doc:"oeil MCP server"

module Log = (val Logs.src_log src)

let server_version = "0.1.0"
let supported_versions = [ "2025-06-18"; "2025-03-26"; "2024-11-05" ]

let instructions =
  "oeil watches OCurrent pipelines (e.g. https://deploy.ci.ocaml.org) through their \
   public web UI. Use deploy_status for a snapshot of how far a pull request or commit \
   got in its deployment, wait_for_deploy to block until it is deployed (or the next \
   change), pipeline_status for the failures and active nodes of the whole pipeline, and \
   job_log to read a job's log. Targets are written owner/repo#PR, owner/repo@SHA or \
   owner/repo:branch."

(* ---------- tool descriptions ---------- *)

let prop ?(typ = "string") desc =
  `Assoc [ ("type", `String typ); ("description", `String desc) ]

let target_props =
  [
    ( "target",
      prop
        "What to follow: #PR, @SHA or :branch of ocaml/ocaml.org (e.g. #4020), or \
         owner/repo#PR, owner/repo@SHA, owner/repo:branch for another repository the \
         deployer handles." );
    ( "branch",
      prop "Branch whose deployment to follow (default: the PR's base branch, or main)."
    );
    ("instance", prop "Base URL of the OCurrent web UI (default: the server's).");
  ]

let tool name desc props required =
  `Assoc
    [
      ("name", `String name);
      ("description", `String desc);
      ( "inputSchema",
        `Assoc
          [
            ("type", `String "object");
            ("properties", `Assoc props);
            ("required", `List (List.map (fun s -> `String s) required));
          ] );
    ]

let tools =
  [
    tool "deploy_status"
      "Snapshot of a pull request's or commit's deployment: the status of each stage \
       (build, push, pull, service, notify), its job, and the overall verdict \
       (in_progress, deployed, failed). For an unmerged PR, says so."
      target_props [ "target" ];
    tool "wait_for_deploy"
      "Block until the deployment of a pull request or commit finishes (until=done, \
       default) or until its next stage change (until=change), or until timeout_s. Waits \
       for an unmerged PR to be merged first. Returns the final snapshot and the events \
       seen."
      (target_props
      @ [
          ("until", prop "\"done\" (default) or \"change\".");
          ( "timeout_s",
            prop ~typ:"number" "Maximum time to wait, in seconds (default 600, max 3600)."
          );
          ("interval_s", prop ~typ:"number" "Polling interval in seconds (default 10).");
        ])
      [ "target" ];
    tool "pipeline_status"
      "The pipeline's failed and active nodes, with error messages and job ids. With \
       repo, expands that repository's group and lists its deployment chains per branch."
      [
        ("repo", prop "owner/repo whose group to expand, e.g. ocaml/ocaml.org.");
        ("instance", prop "Base URL of the OCurrent web UI (default: the server's).");
      ]
      [];
    tool "repo_status"
      "The deploy state of a repository's merged PRs, per deployed branch: every PR not \
       live yet (waiting for the deployer, deploying with its current stage, or failed), \
       whatever its age, and those that went live in the last recent_hours. Also the \
       live commit and the build in flight. A PR's state comes from where its merge \
       commit sits relative to the live commit, not from its merge date."
      [
        ("repo", prop "owner/repo, e.g. ocaml/ocaml.org.");
        ( "recent_hours",
          prop ~typ:"number" "List PRs that went live this many hours back (default 24)."
        );
        ("instance", prop "Base URL of the OCurrent web UI (default: the server's).");
      ]
      [];
    tool "pr_timeline"
      "The life of a pull request, step by step with times and durations: the bot \
       workflow run that opened it (for scraper PRs), the PR, its CI checks, the wait \
       for review and merge, the deployer's stages, and when it went live. Also the \
       share of time spent waiting for review."
      [
        ("repo", prop "owner/repo (default: ocaml/ocaml.org).");
        ("pr", prop ~typ:"integer" "The pull request number.");
        ("instance", prop "Base URL of the OCurrent web UI (default: the server's).");
      ]
      [ "pr" ];
    tool "job_log" "The log of a job (by default its last 200 lines)."
      [
        ("job", prop "Job id, e.g. 2026-11-16/071954-docker-service-f92f02.");
        ("tail", prop ~typ:"integer" "How many lines from the end (default 200, 0 = all).");
        ("instance", prop "Base URL of the OCurrent web UI (default: the server's).");
      ]
      [ "job" ];
  ]

(* ---------- helpers ---------- *)

exception Tool_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Tool_error s)) fmt

let arg_string args k =
  match List.assoc_opt k args with
  | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let arg_float args k =
  match List.assoc_opt k args with
  | Some (`Int i) -> Some (float_of_int i)
  | Some (`Float f) -> Some f
  | Some (`String s) -> float_of_string_opt s
  | _ -> None

let ok_json json =
  `Assoc
    [
      ( "content",
        `List
          [
            `Assoc
              [
                ("type", `String "text");
                ("text", `String (Yojson.Safe.pretty_to_string json));
              ];
          ] );
      ("structuredContent", json);
      ("isError", `Bool false);
    ]

let ok_text text =
  `Assoc
    [
      ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]);
      ("isError", `Bool false);
    ]

let error_result msg =
  `Assoc
    [
      ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String msg) ] ]);
      ("isError", `Bool true);
    ]

type ctx = {
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  web : Web.t;
  github : Github.t;
  default_instance : string;
  repos : (string, Repo_monitor.ctx) Hashtbl.t;
      (** by instance: caches live for the session *)
}

let instance ctx args =
  Instance.make ctx.web
    (Option.value (arg_string args "instance") ~default:ctx.default_instance)

let job_url inst job = Uri.to_string (Instance.job_url inst job)

let resolve ctx args =
  let target =
    match arg_string args "target" with
    | None -> fail "missing argument: target"
    | Some t -> ( match Target.parse t with Ok t -> t | Error e -> fail "%s" e)
  in
  match Resolve.resolve ~github:ctx.github ?branch:(arg_string args "branch") target with
  | Error e -> fail "%s" e
  | Ok r -> r

let pr_json (pr : Github.pull) =
  `Assoc
    [
      ("merged", `Bool false);
      ("pr", `Int pr.number);
      ("title", `String pr.title);
      ("state", `String pr.state);
      ("base", `String pr.base);
    ]

let config inst (r : Resolve.t) ~github ~interval ~timeout =
  {
    Monitor.instance = inst;
    github = Some github;
    target = r.target;
    interval;
    timeout;
    follow_logs = false;
  }

(* ---------- tools ---------- *)

let deploy_status ctx args =
  let inst = instance ctx args in
  match resolve ctx args with
  | `Not_merged pr -> ok_json (pr_json pr)
  | `Ready r -> (
      let errors = ref [] in
      let cfg = config inst r ~github:ctx.github ~interval:0. ~timeout:0. in
      match
        Monitor.snapshot ~clock:ctx.clock ~on_error:(fun e -> errors := e :: !errors) cfg
      with
      | None -> fail "%s" (String.concat "; " (List.rev !errors))
      | Some (t, _) -> ok_json (Json.tracker ~job_url:(job_url inst) ~title:r.title t))

let wait_for_deploy ctx ~progress args =
  let inst = instance ctx args in
  let until_change =
    match arg_string args "until" with
    | None | Some "done" -> false
    | Some "change" -> true
    | Some u -> fail "until must be \"done\" or \"change\", not %S" u
  in
  let timeout =
    Float.min 3600. (Option.value (arg_float args "timeout_s") ~default:600.)
  in
  let interval = Float.max 2. (Option.value (arg_float args "interval_s") ~default:10.) in
  let start = Eio.Time.now ctx.clock in
  let deadline = start +. timeout in
  (* Wait for the merge. *)
  let rec ready () =
    match resolve ctx args with
    | `Ready r -> Some r
    | `Not_merged pr ->
        if Eio.Time.now ctx.clock +. 30. > deadline then None
        else begin
          progress (Printf.sprintf "waiting for #%d to be merged" pr.number);
          Eio.Time.sleep ctx.clock (Float.max interval 30.);
          ready ()
        end
  in
  match ready () with
  | None -> (
      match resolve ctx args with
      | `Not_merged pr ->
          ok_json
            (match pr_json pr with
            | `Assoc l -> `Assoc (l @ [ ("stopped", `String "timeout") ])
            | j -> j)
      | `Ready _ -> fail "merged just now; call again")
  | Some r -> (
      let events = ref [] in
      let polls = ref 0 in
      let changed = ref false in
      let last = ref None in
      let errors = ref [] in
      let handlers =
        {
          Monitor.on_update =
            (fun t evs ->
              incr polls;
              last := Some t;
              if !polls > 1 && evs <> [] then changed := true;
              events := !events @ evs;
              List.iter
                (function
                  | Tracker.Stage { key; after; _ } ->
                      progress
                        (Printf.sprintf "%s: %s" key (Tracker.status_to_string after))
                  | Tracker.Superseded c -> progress ("superseded by " ^ c)
                  | Tracker.Building c -> progress ("building " ^ c)
                  | Tracker.Finished _ -> ())
                evs);
          on_log = (fun ~key:_ ~job:_ _ -> ());
          on_error = (fun e -> errors := e :: !errors);
        }
      in
      let cfg =
        config inst r ~github:ctx.github ~interval
          ~timeout:(Float.max 0. (deadline -. Eio.Time.now ctx.clock))
      in
      let outcome =
        Eio.Switch.run (fun sw ->
            Monitor.run ~sw ~clock:ctx.clock
              ~stop:(fun _ -> until_change && !changed)
              cfg handlers)
      in
      let stopped =
        match outcome with
        | `Deployed | `Failed _ -> "finished"
        | `Timeout -> if until_change && !changed then "change" else "timeout"
      in
      match !last with
      | None -> fail "%s" (String.concat "; " (List.rev !errors))
      | Some t ->
          let base = Json.tracker ~job_url:(job_url inst) ~title:r.title t in
          ok_json
            (match base with
            | `Assoc l ->
                `Assoc
                  (l
                  @ [
                      ("stopped", `String stopped);
                      ("waited_s", `Float (Float.round (Eio.Time.now ctx.clock -. start)));
                      ("events", `List (List.map Json.event !events));
                    ])
            | j -> j))

let pipeline_status ctx args =
  let inst = instance ctx args in
  let repo = arg_string args "repo" in
  let query = Option.map (fun r -> "repo=" ^ r) repo in
  match Instance.pipeline inst ?query () with
  | Error e -> fail "%s" e
  | Ok g ->
      let by p = List.filter (fun n -> p (Svg_graph.effective_state n)) g.nodes in
      let count p = List.length (by p) in
      let chains =
        match repo with
        | None -> []
        | Some repo ->
            [
              ( "chains",
                `Assoc
                  (Chain.branches g ~repo
                  |> List.filter_map (fun branch ->
                      match Chain.stages g ~repo ~branch with
                      | Error _ -> None
                      | Ok stages ->
                          Some
                            ( branch,
                              `List
                                (List.map
                                   (fun (s : Chain.stage) ->
                                     `Assoc
                                       [
                                         ("stage", `String (Chain.describe s));
                                         ( "state",
                                           `String
                                             (State.to_string
                                                (Svg_graph.effective_state s.node)) );
                                         ("job", Json.opt Json.str s.node.job);
                                       ])
                                   stages) ))) );
            ]
      in
      ok_json
        (`Assoc
           ([
              ("instance", `String (Uri.to_string (Instance.base inst)));
              ( "counts",
                `Assoc
                  [
                    ("ok", `Int (count (( = ) State.Ok)));
                    ("active", `Int (count State.is_active));
                    ("failed", `Int (count (( = ) State.Failed)));
                    ("blocked", `Int (count (( = ) State.Blocked)));
                  ] );
              ("failed", `List (List.map Json.node (by (( = ) State.Failed))));
              ("active", `List (List.map Json.node (by State.is_active)));
              ( "collapsed_groups",
                `List
                  (List.filter_map
                     (fun (n : Svg_graph.node) ->
                       Option.map
                         (fun q ->
                           `Assoc
                             [
                               ("expand", `String q);
                               ( "state",
                                 `String (State.to_string (Svg_graph.effective_state n))
                               );
                             ])
                         n.expand)
                     g.nodes) );
            ]
           @ chains))

let job_log ctx args =
  let inst = instance ctx args in
  let job =
    match arg_string args "job" with Some j -> j | None -> fail "missing argument: job"
  in
  let tail =
    Option.value (Option.map int_of_float (arg_float args "tail")) ~default:200
  in
  match Instance.job_log inst job with
  | Error e -> fail "%s" e
  | Ok lines ->
      let n = List.length lines in
      let lines =
        if tail > 0 && n > tail then List.filteri (fun i _ -> i >= n - tail) lines
        else lines
      in
      ok_text (String.concat "\n" lines)

(* oeil is about ocaml.org: the repository is optional. *)
let repo_arg args =
  match arg_string args "repo" with
  | Some r when Target.valid_repo r -> r
  | Some r -> fail "not an owner/repo: %s" r
  | None -> Target.default_repo

(* The repo monitor of an instance: its caches live for the session. *)
let repo_ctx ctx inst =
  let key = Uri.to_string (Instance.base inst) in
  match Hashtbl.find_opt ctx.repos key with
  | Some r -> r
  | None ->
      let jobs = Jobs.create ~clock:ctx.clock inst in
      Jobs.load jobs;
      let r = Repo_monitor.create ~jobs ~github:ctx.github in
      Hashtbl.replace ctx.repos key r;
      r

let repo_status ctx args =
  let inst = instance ctx args in
  let repo = repo_arg args in
  let recent = 3600. *. Option.value (arg_float args "recent_hours") ~default:24. in
  match Repo_monitor.snapshot (repo_ctx ctx inst) ~repo ~recent () with
  | Error e -> fail "%s" e
  | Ok s -> ok_json (Json.repo ~job_url:(job_url inst) s)

let pr_timeline ctx args =
  let inst = instance ctx args in
  let repo = repo_arg args in
  let number =
    match arg_float args "pr" with
    | Some n -> int_of_float n
    | None -> fail "missing argument: pr"
  in
  let rctx = repo_ctx ctx inst in
  let snap =
    match Repo_monitor.snapshot rctx ~repo () with Ok s -> s | Error e -> fail "%s" e
  in
  match Lifecycle_data.pr ctx.github ~repo ~feeds:snap.feeds ~number with
  | Error e -> fail "%s" e
  | Ok (p, github_steps) ->
      let now = Unix.gettimeofday () in
      let found =
        List.find_map
          (fun (b : Repo_monitor.branch) ->
            List.find_map
              (fun (e : Repo_state.entry) ->
                match e.pr with Some q when q.number = number -> Some (b, e) | _ -> None)
              b.entries)
          snap.branches
      in
      let time_of_since s =
        try
          Scanf.sscanf s "%4d-%2d-%2d %2d:%2d.%2d" (fun y mo d h mi se ->
              Option.map Ptime.to_float_s
                (Ptime.of_date_time ((y, mo, d), ((h, mi, se), 0))))
        with _ -> None
      in
      let deploy_steps, state =
        match found with
        | Some (b, ({ state = Live (Some at); _ } as _e)) -> (
            match List.find_opt (fun (d : Repo_state.deploy) -> d.at = at) b.deploys with
            | Some d ->
                ( Lifecycle.of_stages ~now
                    (Deploys.stages_of (Repo_monitor.jobs rctx) ~stages:b.stages d)
                    ~time_of_since
                  @ [
                      Lifecycle.step
                        ?time:(Jobs.seconds_of_key (Jobs.time_key at))
                        ~key:"live" ~status:Lifecycle.Done "live";
                    ],
                  "live" )
            | None -> ([], "live"))
        | Some (b, e) -> (
            let target = { Tracker.repo; branch = b.branch; sha = Some e.commit.sha } in
            let cfg =
              {
                Monitor.instance = inst;
                github = Some ctx.github;
                target;
                interval = 0.;
                timeout = 0.;
                follow_logs = false;
              }
            in
            match Monitor.snapshot ~clock:ctx.clock cfg with
            | Some (t, _) ->
                ( Lifecycle.of_stages ~now (Tracker.stages t) ~time_of_since,
                  Repo_state.state_to_string e.state )
            | None -> ([], Repo_state.state_to_string e.state))
        | None ->
            ( [],
              match p.p_merged with
              | Some _ -> "live (deployed before the recent window)"
              | None -> if p.p_state = "closed" then "closed" else "open" )
      in
      let steps = Lifecycle.with_durations (github_steps @ deploy_steps) in
      ok_json (Json.timeline ~repo ~number ~title:p.p_title ~state steps)

(* ---------- JSON-RPC ---------- *)

exception Cancelled_by_client

let run ~env ~web ~instance:default_instance =
  let clock = (Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t) in
  let stdout = Eio.Stdenv.stdout env in
  let github = Github.make ?token:(Github.token_from_env ()) web in
  let ctx = { clock; web; github; default_instance; repos = Hashtbl.create 2 } in
  let mutex = Eio.Mutex.create () in
  let send (json : Yojson.Safe.t) =
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        Eio.Flow.copy_string (Yojson.Safe.to_string json ^ "\n") stdout)
  in
  let result id r =
    send (`Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", r) ])
  in
  let error id code msg =
    send
      (`Assoc
         [
           ("jsonrpc", `String "2.0");
           ("id", id);
           ("error", `Assoc [ ("code", `Int code); ("message", `String msg) ]);
         ])
  in
  let running : (string, unit -> unit) Hashtbl.t = Hashtbl.create 8 in
  let key id = Yojson.Safe.to_string id in
  let call ~sw id params =
    let open Yojson.Safe.Util in
    let name = try params |> member "name" |> to_string with _ -> "" in
    let args = match params |> member "arguments" with `Assoc l -> l | _ -> [] in
    let token =
      try params |> member "_meta" |> member "progressToken" with _ -> `Null
    in
    let step = ref 0 in
    let progress msg =
      if token <> `Null then begin
        incr step;
        send
          (`Assoc
             [
               ("jsonrpc", `String "2.0");
               ("method", `String "notifications/progress");
               ( "params",
                 `Assoc
                   [
                     ("progressToken", token);
                     ("progress", `Int !step);
                     ("message", `String msg);
                   ] );
             ])
      end
    in
    let run () =
      match name with
      | "deploy_status" -> deploy_status ctx args
      | "wait_for_deploy" -> wait_for_deploy ctx ~progress args
      | "pipeline_status" -> pipeline_status ctx args
      | "job_log" -> job_log ctx args
      | "repo_status" -> repo_status ctx args
      | "pr_timeline" -> pr_timeline ctx args
      | n -> fail "unknown tool: %s" n
    in
    Eio.Fiber.fork ~sw (fun () ->
        match
          Eio.Switch.run (fun sub ->
              Hashtbl.replace running (key id) (fun () ->
                  Eio.Switch.fail sub Cancelled_by_client);
              Fun.protect
                ~finally:(fun () -> Hashtbl.remove running (key id))
                (fun () ->
                  try run () with
                  | Tool_error m -> error_result m
                  | Eio.Cancel.Cancelled _ as e -> raise e
                  | Cancelled_by_client as e -> raise e
                  | e -> error_result (Printexc.to_string e)))
        with
        | r -> result id r
        | exception Cancelled_by_client ->
            Log.info (fun m -> m "request %s cancelled" (key id)))
  in
  let handle ~sw line =
    match Yojson.Safe.from_string line with
    | exception Yojson.Json_error e -> error `Null (-32700) ("parse error: " ^ e)
    | json -> (
        let open Yojson.Safe.Util in
        let id = member "id" json in
        let meth = try member "method" json |> to_string with _ -> "" in
        let params = member "params" json in
        match meth with
        | "initialize" ->
            let requested =
              try params |> member "protocolVersion" |> to_string with _ -> ""
            in
            let version =
              if List.mem requested supported_versions then requested
              else List.hd supported_versions
            in
            result id
              (`Assoc
                 [
                   ("protocolVersion", `String version);
                   ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
                   ( "serverInfo",
                     `Assoc
                       [ ("name", `String "oeil"); ("version", `String server_version) ]
                   );
                   ("instructions", `String instructions);
                 ])
        | "ping" -> result id (`Assoc [])
        | "tools/list" -> result id (`Assoc [ ("tools", `List tools) ])
        | "tools/call" -> call ~sw id params
        | "notifications/cancelled" -> (
            let rid = try member "requestId" params with _ -> `Null in
            match Hashtbl.find_opt running (key rid) with
            | Some cancel -> cancel ()
            | None -> ())
        | m when String.starts_with ~prefix:"notifications/" m -> ()
        | m -> if id <> `Null then error id (-32601) ("method not found: " ^ m))
  in
  let input = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:(16 * 1024 * 1024) in
  (* At end of input, in-flight calls still finish and answer. *)
  Eio.Switch.run (fun sw ->
      let rec loop () =
        match Eio.Buf_read.line input with
        | "" -> loop ()
        | line ->
            handle ~sw line;
            loop ()
        | exception End_of_file -> ()
      in
      loop ())
