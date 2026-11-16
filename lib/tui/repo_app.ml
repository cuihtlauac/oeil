(* oeil repo on a terminal: feeds and active PRs; Enter opens the life of a
   PR or a feed's latest run, and from there the logs. *)
open Oeil
open Oeil_net

type screen = Stages of Watch_screen.t | Life of Lifecycle_screen.t
type nested = { screen : screen; stop : unit -> unit }

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
  let open_row = function
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
