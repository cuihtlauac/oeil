open Oeil
open Oeil_net
open Console

(* ---------- shared options ---------- *)

let instance_arg =
  let doc = "Base URL of the OCurrent web UI." in
  Cmdliner.Arg.(
    value & opt string Instance.default_base & info [ "instance"; "i" ] ~docv:"URL" ~doc)

let setup_logs verbose =
  Logs.set_level (Some (if verbose then Logs.Debug else Logs.Warning))

let verbose_arg =
  Cmdliner.Arg.(value & flag & info [ "verbose"; "v" ] ~doc:"Debug logging.")

let with_env f =
  Eio_main.run @@ fun env ->
  let web = Web.make (Eio.Stdenv.net env) in
  f env web

let mark_of_state (s : State.t) =
  match s with
  | Ok -> View.role `Succeeded (View.text "✔")
  | Running -> View.role `Warning (View.text "●")
  | Ready -> View.role `Warning (View.text "○")
  | Waiting_confirmation -> View.role `Accent (View.text "?")
  | Failed -> View.role `Failed (View.text "✗")
  | Blocked -> View.role `Muted (View.text "·")
  | Unknown _ -> View.text " "

let exit_error msg =
  Fmt.epr "oeil: %s@." msg;
  3

(* ---------- status ---------- *)

let status instance repo all verbose =
  setup_logs verbose;
  Console_tty.setup ();
  with_env @@ fun _env web ->
  let inst = Instance.make web instance in
  let query = Option.map (fun r -> "repo=" ^ r) repo in
  match Instance.pipeline inst ?query () with
  | Error e -> exit_error e
  | Ok g ->
      let count p = List.length (List.filter p g.nodes) in
      let st s (n : Svg_graph.node) = Svg_graph.effective_state n = s in
      Fmt.pr "%a@." View.pp
        (View.hcat ~gap:2
           [
             View.role `Header (View.text (Uri.to_string (Instance.base inst)));
             View.role `Succeeded
               (View.text (Printf.sprintf "%d ok" (count (st State.Ok))));
             View.role `Warning
               (View.text
                  (Printf.sprintf "%d active"
                     (count (fun n -> State.is_active (Svg_graph.effective_state n)))));
             View.role `Failed
               (View.text (Printf.sprintf "%d failed" (count (st State.Failed))));
             View.role `Muted
               (View.text (Printf.sprintf "%d blocked" (count (st State.Blocked))));
           ]);
      let interesting (n : Svg_graph.node) =
        all || Svg_graph.effective_state n <> State.Ok
      in
      let node_label (n : Svg_graph.node) =
        match n.expand with
        | Some q -> Svg_graph.label n ^ " [" ^ q ^ "]"
        | None -> Svg_graph.label n
      in
      let rows =
        List.filter interesting g.nodes
        |> List.map (fun (n : Svg_graph.node) ->
            let label = node_label n in
            [
              mark_of_state (Svg_graph.effective_state n);
              View.text label;
              View.role `Muted (View.text (Option.value n.job ~default:""));
            ])
      in
      if rows <> [] then
        Fmt.pr "%a@." View.pp
          (Table.view
             [
               Table.column ~shrinkable:false (View.text "");
               Table.column (View.text "node");
               Table.column ~shrinkable:false (View.text "job");
             ]
             rows);
      (* Error messages are long: print them in full, one per failed node. *)
      List.iter
        (fun (n : Svg_graph.node) ->
          match n.tooltip with
          | Some t ->
              Fmt.pr "%a@.  %a@." View.pp
                (View.hcat ~gap:1
                   [ View.role `Failed (View.text "✗"); View.text (node_label n) ])
                View.pp
                (View.role `Failed (View.text t))
          | None -> ())
        g.nodes;
      (match repo with
      | None -> ()
      | Some repo ->
          Chain.branches g ~repo
          |> List.iter (fun branch ->
              match Chain.stages g ~repo ~branch with
              | Error _ -> ()
              | Ok stages ->
                  let tree =
                    Tree.v
                      (View.role `Header (View.text (repo ^ " " ^ branch)))
                      (List.map
                         (fun (s : Chain.stage) ->
                           Tree.v
                             (View.hcat ~gap:1
                                [
                                  mark_of_state (Svg_graph.effective_state s.node);
                                  View.text (Chain.describe s);
                                  View.role `Muted
                                    (View.text (Option.value s.node.job ~default:""));
                                ])
                             [])
                         stages)
                  in
                  Fmt.pr "%a@." View.pp (Tree.view tree)));
      0

let status_cmd =
  let open Cmdliner in
  let repo =
    Arg.(
      value
      & opt (some string) None
      & info [ "repo"; "r" ] ~docv:"OWNER/REPO"
          ~doc:"Expand this repository's group and show its deployment chains.")
  in
  let all =
    Arg.(value & flag & info [ "all"; "a" ] ~doc:"List nodes that are ok, too.")
  in
  Cmd.v
    (Cmd.info "status"
       ~doc:"Show the pipeline: failures, active nodes, deployment chains.")
    Term.(const status $ instance_arg $ repo $ all $ verbose_arg)

(* ---------- log ---------- *)

let log instance job follow verbose =
  setup_logs verbose;
  with_env @@ fun _env web ->
  let inst = Instance.make web instance in
  let job =
    (* Accept a full URL or "/job/…" too. *)
    match String.split_on_char '/' job |> List.rev with
    | id :: date :: _ when String.length date = 10 -> date ^ "/" ^ id
    | _ -> job
  in
  let r =
    if follow then Instance.follow_job inst job ~on_line:(fun l -> print_endline l)
    else Result.map (List.iter print_endline) (Instance.job_log inst job)
  in
  match r with Ok () -> 0 | Error e -> exit_error e

let log_cmd =
  let open Cmdliner in
  let job =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"JOB" ~doc:"Job id, e.g. 2026-11-16/071954-docker-service-f92f02.")
  in
  let follow =
    Arg.(value & flag & info [ "follow"; "f" ] ~doc:"Keep printing while the job runs.")
  in
  Cmd.v
    (Cmd.info "log" ~doc:"Print a job's log.")
    Term.(const log $ instance_arg $ job $ follow $ verbose_arg)

(* ---------- watch ---------- *)

let short d =
  (* "sha256:67a2378…" *)
  if String.length d > 19 then String.sub d 0 19 ^ "…" else d

let short_sha s = if String.length s > 9 then String.sub s 0 9 else s

let watch_ui root =
  let tasks : (string, Task.t) Hashtbl.t = Hashtbl.create 8 in
  let waiting = ref (Some (Task.start root "waiting for the deployer to pick it up")) in
  let end_waiting () =
    match !waiting with
    | Some t ->
        Task.succeed t ~message:"picked up by the deployer";
        waiting := None
    | None -> ()
  in
  let task_for (st : Tracker.stage) =
    match Hashtbl.find_opt tasks st.key with
    | Some t -> t
    | None ->
        end_waiting ();
        let t = Task.start ~log_lines:6 root (Chain.kind_label st.kind) in
        Hashtbl.replace tasks st.key t;
        t
  in
  let label (st : Tracker.stage) extra =
    let base = Chain.kind_label st.kind in
    if extra = "" then base else base ^ "  " ^ extra
  in
  let on_update tracker events =
    List.iter
      (function
        | Tracker.Superseded c ->
            Task.log root
              (Printf.sprintf "superseded by %s, which includes the target commit"
                 (short_sha c))
        | Tracker.Building _ | Tracker.Stage _ | Tracker.Finished _ -> ())
      events;
    let built, published = Tracker.digests tracker in
    List.iter
      (fun (st : Tracker.stage) ->
        let job = Option.value st.job ~default:"" in
        match st.status with
        | Tracker.Pending -> ()
        | Blocked -> (
            match Hashtbl.find_opt tasks st.key with
            | Some t -> Task.set_label t (label st "blocked by an upstream failure")
            | None -> ())
        | Queued -> Task.set_label (task_for st) (label st "queued")
        | Needs_confirmation ->
            Task.set_label (task_for st) (label st "needs confirmation")
        | Running -> Task.set_label (task_for st) (label st job)
        | Done ->
            let extra =
              match st.kind with
              | Chain.Build _ -> (
                  (match Tracker.commit tracker with Some c -> short_sha c | None -> "")
                  ^ match built with d :: _ -> " → " ^ short d | [] -> "")
              | Chain.Push _ -> (
                  match published with Some d -> "→ " ^ short d | None -> "")
              | _ -> job
            in
            Task.succeed (task_for st) ~message:(label st extra)
        | Unchanged ->
            Task.succeed (task_for st) ~message:(label st "unchanged, already live")
        | Failed m ->
            let t = task_for st in
            Task.log ~level:`Error t m;
            Task.fail t ~message:(label st ("failed  " ^ job)))
      (Tracker.stages tracker)
  in
  let on_log ~key ~job:_ line =
    match Hashtbl.find_opt tasks key with Some t -> Task.log t line | None -> ()
  in
  let finish () = end_waiting () in
  (on_update, on_log, finish)

let summary tracker =
  let rows =
    List.map
      (fun (st : Tracker.stage) ->
        let mark =
          match st.status with
          | Tracker.Done | Unchanged -> View.role `Succeeded (View.text "✔")
          | Failed _ -> View.role `Failed (View.text "✗")
          | Running | Queued | Needs_confirmation -> View.role `Warning (View.text "●")
          | Pending | Blocked -> View.role `Muted (View.text "·")
        in
        [
          mark;
          View.text (Chain.kind_label st.kind);
          View.text
            (match st.status with
            | Failed _ -> "failed"
            | s -> Tracker.status_to_string s);
          View.role `Muted (View.text (Option.value st.job ~default:""));
        ])
      (Tracker.stages tracker)
  in
  Table.view
    [
      Table.column ~shrinkable:false (View.text "");
      Table.column (View.text "stage");
      Table.column ~shrinkable:false (View.text "status");
      Table.column ~shrinkable:false (View.text "job");
    ]
    rows

let interactive instance target branch interval timeout =
  with_env @@ fun env web ->
  let github = Github.make ?token:(Github.token_from_env ()) web in
  match
    Oeil_tui.App.run ~env ~web ~instance ~github ~target ?branch ~interval ~timeout ()
  with
  | `Deployed -> 0
  | `Failed _ -> 1
  | `Timeout -> 2
  | `Error e -> exit_error e
  | `Quit -> 4

let watch instance target branch interval timeout progress no_logs rows verbose =
  setup_logs verbose;
  match Target.parse target with
  | Error e -> exit_error e
  | Ok target
    when (not rows) && progress = `Auto && Unix.isatty Unix.stdin
         && Option.is_some (Console_eio.size Fmt.stderr) ->
      Console_tty.setup ();
      interactive instance target branch interval timeout
  | Ok target -> (
      with_env @@ fun env web ->
      let clock = Eio.Stdenv.clock env in
      let github = Github.make ?token:(Github.token_from_env ()) web in
      let inst = Instance.make web instance in
      let last = ref None in
      let result =
        Console_eio.run ~clock:(Eio.Stdenv.mono_clock env) ~progress
          ~label:(Target.to_string target) (fun root ->
            (* Resolve the target, waiting for a PR to be merged. *)
            let rec resolve merge_task =
              match Resolve.resolve ~github ?branch target with
              | Error e ->
                  Option.iter (fun t -> Task.fail t) merge_task;
                  Error e
              | Ok (`Ready r) ->
                  Option.iter (fun t -> Task.succeed t ~message:"merged") merge_task;
                  Ok r
              | Ok (`Not_merged (pr : Github.pull)) ->
                  let t =
                    match merge_task with
                    | Some t -> t
                    | None ->
                        Task.start root
                          (Printf.sprintf "waiting for #%d (%s) to be merged" pr.number
                             pr.title)
                  in
                  Eio.Time.sleep clock (max interval 30.);
                  resolve (Some t)
            in
            match resolve None with
            | Error e ->
                Task.fail root ~message:e;
                Error e
            | Ok r ->
                Task.log root r.title;
                let on_update, on_log, finish = watch_ui root in
                let cfg =
                  {
                    Monitor.instance = inst;
                    github = Some github;
                    target = r.target;
                    interval;
                    timeout;
                    follow_logs = not no_logs;
                  }
                in
                let handlers =
                  {
                    Monitor.on_update =
                      (fun t ev ->
                        last := Some t;
                        on_update t ev);
                    on_log;
                    on_error = (fun e -> Logs.warn (fun m -> m "%s" e));
                  }
                in
                let outcome =
                  Eio.Switch.run (fun sw -> Monitor.run ~sw ~clock cfg handlers)
                in
                finish ();
                (match outcome with
                | `Deployed -> Task.succeed root ~message:("deployed: " ^ r.title)
                | `Failed k -> Task.fail root ~message:("deployment failed at " ^ k)
                | `Timeout -> Task.fail root ~message:"timed out");
                Ok outcome)
      in
      (match !last with Some t -> Fmt.pr "%a@." View.pp (summary t) | None -> ());
      match result with
      | Error _ -> 3
      | Ok `Deployed -> 0
      | Ok (`Failed _) -> 1
      | Ok `Timeout -> 2)

let watch_cmd =
  let open Cmdliner in
  let target =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"TARGET"
          ~doc:
            "What to follow: $(b,owner/repo#PR) (waits for the merge), \
             $(b,owner/repo@SHA), or $(b,owner/repo:branch) (the branch's latest build).")
  in
  let branch =
    Arg.(
      value
      & opt (some string) None
      & info [ "branch"; "b" ] ~docv:"BRANCH"
          ~doc:"Branch whose deployment to follow (default: the PR's base, or main).")
  in
  let interval =
    Arg.(
      value & opt float 10. & info [ "interval" ] ~docv:"SECONDS" ~doc:"Polling interval.")
  in
  let timeout =
    Arg.(
      value & opt float 3600.
      & info [ "timeout" ] ~docv:"SECONDS" ~doc:"Give up after this long.")
  in
  let progress =
    Arg.(
      value
      & opt (enum Console.Progress.all) `Auto
      & info [ "progress" ] ~docv:"MODE"
          ~doc:"How to show progress: auto, tty, plain, quiet or json.")
  in
  let no_logs =
    Arg.(value & flag & info [ "no-logs" ] ~doc:"Do not stream the logs of running jobs.")
  in
  let rows =
    Arg.(
      value & flag
      & info [ "rows" ]
          ~doc:
            "Print scrolling rows instead of the interactive view, as on a pipe. The \
             interactive view is used when standard input and error are terminals and \
             $(b,--progress) is auto.")
  in
  let exits =
    Cmd.Exit.
      [
        info 0 ~doc:"the commit is deployed.";
        info 1 ~doc:"the deployment failed.";
        info 2 ~doc:"timed out.";
        info 3 ~doc:"bad target or unreachable service.";
        info 4 ~doc:"quit (q) before the deployment finished.";
      ]
  in
  Cmd.v
    (Cmd.info "watch" ~exits ~doc:"Follow a pull request or commit until it is deployed.")
    Term.(
      const watch $ instance_arg $ target $ branch $ interval $ timeout $ progress
      $ no_logs $ rows $ verbose_arg)

(* ---------- dashboard ---------- *)

let repo = Target.default_repo

let dashboard instance recent every once verbose =
  setup_logs verbose;
  with_env @@ fun env web ->
  let github = Github.make ?token:(Github.token_from_env ()) web in
  let every =
    match every with Some e -> e | None -> if Github.has_token github then 20. else 300.
  in
  let recent = recent *. 3600. in
  Console_tty.setup ();
  if (not once) && Unix.isatty Unix.stdin && Oeil_tui.Screen.available () then
    match Oeil_tui.Repo_app.run ~env ~web ~instance ~github ~repo ~recent ~every () with
    | Ok () -> 0
    | Error e -> exit_error e
  else
    let inst = Instance.make web instance in
    let jobs = Jobs.create ~clock:(Eio.Stdenv.clock env) inst in
    Jobs.load jobs;
    let ctx = Repo_monitor.create ~jobs ~github in
    match Repo_monitor.snapshot ctx ~repo ~recent () with
    | Error e -> exit_error e
    | Ok s ->
        let main =
          List.find_opt (fun (b : Repo_monitor.branch) -> b.branch = "main") s.branches
        in
        let upstreams =
          Upstreams.fetch (Upstreams.create ~monitor:ctx ~web) ~gh:github ~ocamlorg:main
        in
        let m = Oeil_tui.Repo_model.with_snapshot (Oeil_tui.Repo_model.v ~repo) s in
        Fmt.pr "%a@." View.pp
          (Oeil_tui.Repo_render.final { m with upstreams = Some upstreams });
        0

let dashboard_term =
  let open Cmdliner in
  let recent =
    Arg.(
      value & opt float 24.
      & info [ "recent" ] ~docv:"HOURS"
          ~doc:"Also list the PRs that went live in the last $(docv) hours.")
  in
  let every =
    Arg.(
      value
      & opt (some float) None
      & info [ "every" ] ~docv:"SECONDS"
          ~doc:"Refresh period (default 20 s, 300 s without a GitHub token).")
  in
  let once =
    Arg.(
      value & flag
      & info [ "once"; "rows" ]
          ~doc:"Print the dashboard once instead of the interactive view.")
  in
  Term.(const dashboard $ instance_arg $ recent $ every $ once $ verbose_arg)

let dashboard_doc =
  "ocaml.org's dashboard: its upstreams (docs CI, opam health, compiler manuals), its \
   feeds (scrapers and other scheduled or bot workflows), and every merged PR not live \
   yet (waiting, deploying or failed), whatever its age, or live since recently. On a \
   terminal, Enter opens a row's timeline, and a step's log."

let dashboard_cmd =
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "dashboard" ~doc:dashboard_doc) dashboard_term

(* ---------- mcp ---------- *)

let mcp instance verbose =
  setup_logs verbose;
  with_env @@ fun env web ->
  Oeil_mcp.Server.run ~env ~web ~instance;
  0

let mcp_cmd =
  let open Cmdliner in
  Cmd.v
    (Cmd.info "mcp" ~doc:"Run an MCP server on stdin/stdout.")
    Term.(const mcp $ instance_arg $ verbose_arg)

let () =
  let open Cmdliner in
  let doc =
    "Watch ocaml.org's way to production — its PRs, deploys, upstreams and CI — from a \
     terminal or an MCP client. Without a command, opens the dashboard."
  in
  exit
    (Cmd.eval'
       (Cmd.group ~default:dashboard_term
          (Cmd.info "oeil" ~version:"0.1.0" ~doc)
          [ dashboard_cmd; watch_cmd; status_cmd; log_cmd; mcp_cmd ]))
