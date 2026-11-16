(* oeil repo on a terminal: active PRs; Enter opens a PR's deployment. *)
open Oeil
open Oeil_net

type nested = { screen : Watch_screen.t; stop : unit -> unit }

let run ~env ~web ~instance ~github ~repo ~recent ~every () =
  let clock = Eio.Stdenv.clock env in
  let inst = Instance.make web instance in
  let jobs = Jobs.create ~clock inst in
  Jobs.load jobs;
  let ctx = Repo_monitor.create ~jobs ~github in
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
          | Ok s -> model := Repo_model.with_snapshot !model s
          | Error e -> model := { !model with refreshing = false; last_error = Some e });
          dirty ();
          Eio.Fiber.first
            (fun () -> Eio.Time.sleep clock every)
            (fun () -> Eio.Stream.take refresh_now);
          loop ()
        in
        loop ())
  in
  let open_row (row : Repo_model.row) =
    let sw = Option.get !sw_ref in
    let title =
      match row.entry.pr with
      | Some p -> Printf.sprintf "%s#%d %s → %s" repo p.number p.title row.branch
      | None ->
          Printf.sprintf "%s %s %s → %s" repo
            (Render.short_sha row.entry.commit.sha)
            row.entry.commit.message row.branch
    in
    let target = { Tracker.repo; branch = row.branch; sha = Some row.entry.commit.sha } in
    (* A live PR shows the deploy that made it live; any other follows its
       commit through the current one. *)
    let past =
      match (row.entry.state, !model.snapshot) with
      | Live (Some at), Some snap -> (
          match
            List.find_opt
              (fun (b : Repo_monitor.branch) -> b.branch = row.branch)
              snap.branches
          with
          | Some b -> (
              match
                List.find_opt (fun (d : Repo_state.deploy) -> d.at = at) b.deploys
              with
              | Some d -> Some (b.stages, d)
              | None -> None)
          | None -> None)
      | _ -> None
    in
    Eio.Fiber.fork ~sw (fun () ->
        try
          Eio.Switch.run (fun sub ->
              let w = Watch_screen.create ~sw:sub ~inst ~title ~dirty:!dirty_ref in
              nested := Some { screen = w; stop = (fun () -> Eio.Switch.fail sub Exit) };
              (match past with
              | Some (stages, d) ->
                  let when_ =
                    match Jobs.seconds_of_key (Jobs.time_key d.at) with
                    | Some t ->
                        " · live since " ^ Render.clock_time ~now:(Unix.gettimeofday ()) t
                    | None -> ""
                  in
                  let title = title ^ when_ in
                  Watch_screen.set w (fun m -> { m with title; phase = Resolving });
                  Watch_screen.show_deploy w ~title (Deploys.stages_of jobs ~stages d)
              | None ->
                  Watch_screen.monitor w ~clock ~github ~target ~title ~interval:10.
                    ~timeout:86400.);
              (* Keep following logs after the deployment ended. *)
              Eio.Fiber.await_cancel ())
        with Exit -> ())
  in
  let on_key ~page k =
    match !nested with
    | Some n -> (
        match Watch_screen.key n.screen ~page k with
        | `Quit ->
            nested := None;
            n.stop ();
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
    | Some n -> Watch_screen.view n.screen
    | None -> Repo_render.view !model
  in
  let final () = Repo_render.final !model in
  let r = Screen.run ~env ~view ~final ~start ~on_key in
  Jobs.save jobs;
  r
