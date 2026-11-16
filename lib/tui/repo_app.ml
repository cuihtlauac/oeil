(* oeil repo on a terminal: feeds and active PRs; Enter opens the life of a
   PR or a feed's latest run, and from there the logs. *)
open Oeil
open Oeil_net
open Console

type screen = Stages of Watch_screen.t | Life of Lifecycle_screen.t
type nested = { screen : screen; stop : unit -> unit }

let run ~env ~web ~instance ~github ~repo ~recent ~every () =
  let clock = Eio.Stdenv.clock env in
  let inst = Instance.make web instance in
  let jobs = Jobs.create ~clock inst in
  Jobs.load jobs;
  let ctx = Repo_monitor.create ~jobs ~github in
  let up_ctx = Upstreams.create ~monitor:ctx ~web in
  let model = ref (Repo_model.v ~repo) in
  let nested = ref None in
  let refresh_now = Eio.Stream.create 1 in
  let sw_ref = ref None and dirty_ref = ref (fun () -> ()) in
  let start ~sw ~dirty =
    sw_ref := Some sw;
    dirty_ref := dirty;
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let rec loop () =
          model := { !model with refreshing = true };
          dirty ();
          (match Repo_monitor.snapshot ctx ~repo ~recent () with
          | Ok s ->
              model := Repo_model.with_snapshot !model s;
              dirty ();
              (* Then what ocaml.org is built from. *)
              let main =
                List.find_opt
                  (fun (b : Repo_monitor.branch) -> b.branch = "main")
                  s.branches
              in
              model :=
                {
                  !model with
                  upstreams = Some (Upstreams.fetch up_ctx ~gh:github ~ocamlorg:main);
                }
          | Error e -> model := { !model with refreshing = false; last_error = Some e });
          dirty ();
          Eio.Fiber.first
            (fun () -> Eio.Time.sleep clock every)
            (fun () -> Eio.Stream.take refresh_now);
          loop ()
        in
        loop ())
  in
  (* Run [fill] in a sub-switch for the screen it makes; leaving the screen
     cancels it. *)
  let open_screen make fill =
    let sw = Option.get !sw_ref in
    Eio.Fiber.fork ~sw (fun () ->
        try
          Eio.Switch.run (fun sub ->
              let s = make sub in
              nested := Some { screen = s; stop = (fun () -> Eio.Switch.fail sub Exit) };
              fill s;
              (* Keep following logs after it ended. *)
              Eio.Fiber.await_cancel ())
        with Exit -> ())
  in
  let snapshot () = !model.snapshot in
  let feeds () = match snapshot () with Some s -> s.feeds | None -> [] in
  let branch name =
    Option.bind (snapshot ()) (fun s ->
        List.find_opt (fun (b : Repo_monitor.branch) -> b.branch = name) s.branches)
  in
  let entry_of_pr number =
    Option.bind (snapshot ()) (fun s ->
        List.find_map
          (fun (b : Repo_monitor.branch) ->
            List.find_map
              (fun (e : Repo_state.entry) ->
                match e.pr with
                | Some p when p.number = number -> Some (b.branch, e)
                | _ -> None)
              b.entries)
          s.branches)
  in
  (* The life of PR [number]: GitHub, then its deploy. *)
  let open_pr ~title ~number ~entry =
    let deploy =
      match entry with
      | Some (br, (e : Repo_state.entry)) -> (
          match e.state with
          | Live (Some at) -> (
              match branch br with
              | Some b -> (
                  match
                    List.find_opt (fun (d : Repo_state.deploy) -> d.at = at) b.deploys
                  with
                  | Some d -> `Past (b.stages, d)
                  | None -> `None)
              | None -> `None)
          | Live None -> `None
          | Waiting | Deploying _ | Failed _ ->
              `Follow { Tracker.repo; branch = br; sha = Some e.commit.sha })
      | None -> `None
    in
    open_screen
      (fun sw ->
        Life (Lifecycle_screen.create ~sw ~dirty:!dirty_ref ~repo ~gh:github ~inst ~title))
      (function
        | Life t ->
            Lifecycle_screen.fill_pr t ~clock ~jobs ~feeds:(feeds ()) ~number ~deploy
        | Stages _ -> ())
  in
  let life ~title fill =
    open_screen
      (fun sw ->
        Life (Lifecycle_screen.create ~sw ~dirty:!dirty_ref ~repo ~gh:github ~inst ~title))
      (function Life t -> fill t | Stages _ -> ())
  in
  let module L = Lifecycle in
  let state_status (s : State.t) =
    match s with
    | State.Ok -> L.Done
    | Running -> L.Running
    | Ready | Waiting_confirmation -> L.Waiting
    | Failed -> L.Failed
    | Blocked | Unknown _ -> L.Pending
  in
  let open_upstream (k : Repo_model.upstream) =
    match !model.upstreams with
    | None -> ()
    | Some u -> (
        let now = Unix.gettimeofday () in
        match k with
        | Docs ->
            (* dill's pipeline, node by node, then docs-ci's own deploy. *)
            let nodes =
              match u.docs.pipeline with
              | Some g ->
                  L.step ~key:"pipeline" ~status:L.Done ~detail:Upstreams.docs_ci_base
                    "pipeline"
                  :: List.mapi
                       (fun i (n : Svg_graph.node) ->
                         (* A collapsed group is labelled "+": say what it holds. *)
                         let n =
                           match (n.label, n.expand) with
                           | [ "+" ], Some q -> { n with label = [ "group " ^ q ] }
                           | _ -> n
                         in
                         L.step ~depth:1 ~key:(Printf.sprintf "node:%d" i)
                           ~status:(state_status (Svg_graph.effective_state n))
                           ~detail:
                             (Option.value n.tooltip
                                ~default:(Option.value n.job ~default:""))
                           ?log:
                             (Option.map
                                (fun job ->
                                  L.Ocurrent_job { base = Upstreams.docs_ci_base; job })
                                n.job)
                           (Svg_graph.label n))
                       (* Gates are drawn as unlabelled circles. *)
                       (List.filter (fun (n : Svg_graph.node) -> n.label <> []) g.nodes)
              | None -> []
            in
            life ~title:"docs CI (dill)" (fun t ->
                Lifecycle_screen.show t
                  ~status:(Render.muted "ocurrent/ocaml-docs-ci · dill")
                  nodes;
                match u.docs.deploy.d_state with
                | Some st ->
                    Lifecycle_screen.set_deploy t ~now (Tracker.stages st.tracker)
                | None -> ())
        | Health -> (
            let feed =
              List.find_opt
                (fun (f : Ci.feed) -> f.branch = Some "bot/opam-health")
                (feeds ())
            in
            match Option.bind feed Ci.last_pr with
            | Some n ->
                open_pr
                  ~title:(Printf.sprintf "opam health → %s#%d" repo n)
                  ~number:n ~entry:(entry_of_pr n)
            | None -> ())
        | Manuals -> (
            match u.manuals.commit with
            | None -> ()
            | Some c ->
                let main = branch "main" in
                let d =
                  Option.bind main (fun (b : Repo_monitor.branch) ->
                      match b.deploys with d :: _ -> Some d | [] -> None)
                in
                let steps =
                  [
                    L.step ?time:(Ci.time_of_iso c.date) ~key:"commit" ~status:L.Done
                      ~detail:(Render.short_sha c.sha ^ " " ^ c.message)
                      ("commit to " ^ Upstreams.manuals_repo);
                  ]
                  @
                  match (d, u.manuals.included) with
                  | Some d, Some true ->
                      [
                        L.step ?time:u.manuals.live_build ~key:"build" ~status:L.Done
                          ~detail:"git clone at image build time"
                          ?log:(Option.map (fun j -> L.Deployer_job j) d.build_job)
                          "cloned by the ocaml.org build";
                        L.step
                          ?time:(Jobs.seconds_of_key (Jobs.time_key d.at))
                          ~key:"live" ~status:L.Done ~log:(L.Deployer_job d.at)
                          "live on ocaml.org";
                      ]
                  | _ ->
                      [
                        L.step ~key:"build" ~status:L.Waiting
                          ~detail:"the daily rebuild, or the next merge, picks it up"
                          "waiting for the next ocaml.org build";
                      ]
                in
                life ~title:"compiler manuals" (fun t ->
                    Lifecycle_screen.show t
                      ~status:
                        (if u.manuals.included = Some true then
                           View.role `Succeeded (View.text "✔ live")
                         else View.role `Warning (View.text "waiting"))
                      steps)))
  in
  let open_row = function
    | Repo_model.Upstream k -> open_upstream k
    | Repo_model.Pr { branch = br; entry = { pr = Some p; _ } as e } ->
        open_pr
          ~title:(Printf.sprintf "%s#%d %s → %s" repo p.number p.title br)
          ~number:p.number
          ~entry:(Some (br, e))
    | Pr { branch = br; entry = e } ->
        (* A commit pushed without a PR: its deploy only. *)
        let title =
          Printf.sprintf "%s %s %s → %s" repo
            (Render.short_sha e.commit.sha)
            e.commit.message br
        in
        let target = { Tracker.repo; branch = br; sha = Some e.commit.sha } in
        open_screen
          (fun sw -> Stages (Watch_screen.create ~sw ~inst ~title ~dirty:!dirty_ref))
          (function
            | Stages w ->
                Watch_screen.monitor w ~clock ~github ~target ~title ~interval:10.
                  ~timeout:86400.
            | Life _ -> ())
    | Feed f -> (
        (* Its latest PR, from the run that opened it to production; or, if
           it never opened one, its latest run. *)
        match (Ci.last_pr f, f.runs) with
        | Some n, _ ->
            let title = Printf.sprintf "%s · %s → #%d" repo f.name n in
            open_pr ~title ~number:n ~entry:(entry_of_pr n)
        | None, run :: _ ->
            let title = Printf.sprintf "%s · %s, latest run" repo f.name in
            open_screen
              (fun sw ->
                Life
                  (Lifecycle_screen.create ~sw ~dirty:!dirty_ref ~repo ~gh:github ~inst
                     ~title))
              (function
                | Life t -> Lifecycle_screen.fill_run t ~feed:f run | Stages _ -> ())
        | None, [] -> ())
  in
  let leave n =
    nested := None;
    n.stop ()
  in
  let on_key ~page k =
    match !nested with
    | Some ({ screen = Stages w; _ } as n) -> (
        match Watch_screen.key w ~page k with
        | `Quit ->
            leave n;
            `Continue
        | `Continue -> `Continue)
    | Some ({ screen = Life t; _ } as n) -> (
        match Lifecycle_screen.key t ~page k with
        | `Quit ->
            leave n;
            `Continue
        | `Continue -> `Continue)
    | None -> (
        let m, action = Repo_model.key !model k in
        model := m;
        match action with
        | Repo_model.Quit -> `Quit
        | Open row ->
            open_row row;
            `Continue
        | Refresh ->
            ignore (Eio.Stream.take_nonblocking refresh_now);
            Eio.Stream.add refresh_now ();
            `Continue
        | Nothing -> `Continue)
  in
  let view () =
    match !nested with
    | Some { screen = Stages w; _ } -> Watch_screen.view w
    | Some { screen = Life t; _ } -> Lifecycle_screen.view t
    | None -> Repo_render.view !model
  in
  let final () = Repo_render.final !model in
  let r = Screen.run ~env ~view ~final ~start ~on_key in
  Jobs.save jobs;
  r
