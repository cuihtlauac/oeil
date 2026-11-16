open Oeil
open Oeil_net
open Console

let available ~stdin =
  Option.is_some (Console_eio.size Fmt.stderr)
  && Console_eio.Prompt.interactive (Console_eio.Prompt.v stdin)

type result = [ `Deployed | `Failed of string | `Timeout | `Quit | `Error of string ]

let run ~env ~web ~instance ~github ~(target : Target.t) ?branch ~interval ~timeout () :
    result =
  let clock = Eio.Stdenv.clock env in
  let inst = Instance.make web instance in
  let logs = Log_store.create () in
  let model =
    ref (Model.v ~title:(Target.to_string target) ~now:(Unix.gettimeofday ()))
  in
  let dirty = ref true in
  let set f =
    model := f !model;
    dirty := true
  in
  let size =
    match Console_eio.size Fmt.stderr with Some s -> s | None -> fun () -> (80, 24)
  in
  let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
  let t0 = Unix.gettimeofday () in
  let outcome = ref None in
  let r =
    Console_eio.Prompt.keys prompt @@ fun input ->
    let d =
      Display.v ~size
        ~now:(fun () -> Unix.gettimeofday () -. t0)
        Fmt.stderr (Render.view ~logs !model)
    in
    let closed = ref false in
    let close () =
      if not !closed then begin
        closed := true;
        Display.set_view d (Render.final ~now:(Unix.gettimeofday ()) !model);
        Display.close d
      end
    in
    at_exit close;
    Fun.protect ~finally:close @@ fun () ->
    let refresh () =
      dirty := false;
      Display.set_view d (Render.view ~logs !model)
    in
    Eio.Switch.run @@ fun sw ->
    (* Fetch, or follow, a job's log once. *)
    let open_log job =
      match Log_store.find logs job with
      | Some _ -> ()
      | None ->
          let l = Log_store.add logs job in
          Eio.Fiber.fork_daemon ~sw (fun () ->
              (match
                 Instance.follow_job inst job ~on_line:(fun line ->
                     Log_store.push l line;
                     dirty := true)
               with
              | Ok () -> l.complete <- true
              | Error e -> l.error <- Some e);
              dirty := true;
              `Stop_daemon)
    in
    (* A log view follows the stage: when its job changes, fetch the new one. *)
    let follow_open_log () =
      match !model.mode with
      | Model.Log { key; _ } -> (
          match Model.stage !model key with
          | Some { job = Some job; _ } -> open_log job
          | _ -> ())
      | Summary -> ()
    in
    (* Redraws: at once on a key, else ten times a second. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let rec loop () =
          if !dirty then refresh () else Display.redraw d;
          Eio.Time.sleep clock 0.1;
          loop ()
        in
        loop ());
    let watch () =
      let rec resolve () =
        match Resolve.resolve ~github ?branch target with
        | Error e ->
            set (fun m -> { m with phase = Error e });
            None
        | Ok (`Ready r) -> Some r
        | Ok (`Not_merged (pr : Github.pull)) ->
            set (fun m ->
                {
                  m with
                  phase =
                    Waiting_merge
                      (Printf.sprintf "waiting for #%d to be merged" pr.number);
                });
            Eio.Time.sleep clock (Float.max interval 30.);
            resolve ()
      in
      match resolve () with
      | None -> outcome := Some (`Error "cannot resolve the target")
      | Some r ->
          set (fun m -> { m with title = r.title; phase = Watching });
          let cfg =
            {
              Monitor.instance = inst;
              github = Some github;
              target = r.target;
              interval;
              timeout;
              follow_logs = false;
            }
          in
          let handlers =
            {
              Monitor.on_update =
                (fun tr _ ->
                  set (fun m ->
                      {
                        (Model.with_tracker m tr) with
                        last_poll = Some (Unix.gettimeofday ());
                        last_error = None;
                      });
                  follow_open_log ());
              on_log = (fun ~key:_ ~job:_ _ -> ());
              on_error = (fun e -> set (fun m -> { m with last_error = Some e }));
            }
          in
          let o = Monitor.run ~sw ~clock cfg handlers in
          outcome := Some (o :> result);
          set (fun m -> { m with phase = Finished o })
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        watch ();
        `Stop_daemon);
    (* Keys, until q. *)
    let rec keys () =
      let page = max 1 (snd (size ()) - 6) in
      match Keys.read input with
      | exception End_of_file -> ()
      | k -> (
          let m, action = Model.key !model ~page k in
          model := m;
          (match action with Model.Open_log job -> open_log job | Nothing | Quit -> ());
          refresh ();
          match action with Quit -> () | _ -> keys ())
    in
    keys ()
  in
  match r with
  | Error e -> `Error (Fmt.str "%a" Console_eio.Prompt.pp_error e)
  | Ok () -> ( match !outcome with Some o -> o | None -> `Quit)
