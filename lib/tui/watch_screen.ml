(* The stages of one commit's deployment, with their logs. *)
open Oeil
open Oeil_net

type outcome = [ `Deployed | `Failed of string | `Timeout ]

type t = {
  model : Model.t ref;
  logs : Log_store.t;
  inst : Instance.t;
  sw : Eio.Switch.t;
  dirty : unit -> unit;
  mutable outcome : outcome option;
}

let create ~sw ~inst ~title ~dirty =
  {
    model = ref (Model.v ~title ~now:(Unix.gettimeofday ()));
    logs = Log_store.create ();
    inst;
    sw;
    dirty;
    outcome = None;
  }

let model t = !(t.model)
let outcome t = t.outcome

let set t f =
  t.model := f !(t.model);
  t.dirty ()

(* Fetch, or follow, a job's log once. *)
let open_log t job =
  match Log_store.find t.logs job with
  | Some _ -> ()
  | None ->
      let l = Log_store.add t.logs job in
      Eio.Fiber.fork_daemon ~sw:t.sw (fun () ->
          (match
             Instance.follow_job t.inst job ~on_line:(fun line ->
                 Log_store.push l line;
                 t.dirty ())
           with
          | Ok () -> l.complete <- true
          | Error e -> l.error <- Some e);
          t.dirty ();
          `Stop_daemon)

(* A log view follows its stage: when the stage's job changes, fetch the new
   one. *)
let follow_open_log t =
  match !(t.model).mode with
  | Model.Log { key; _ } -> (
      match Model.stage !(t.model) key with
      | Some { job = Some job; _ } -> open_log t job
      | _ -> ())
  | Summary -> ()

(* Follow [target] until its deployment ends. *)
let monitor t ~clock ~github ~(target : Tracker.target) ~title ~interval ~timeout =
  set t (fun m -> { m with title; phase = Watching });
  let cfg =
    {
      Monitor.instance = t.inst;
      github = Some github;
      target;
      interval;
      timeout;
      follow_logs = false;
    }
  in
  let handlers =
    {
      Monitor.on_update =
        (fun tr _ ->
          set t (fun m ->
              {
                (Model.with_tracker m tr) with
                last_poll = Some (Unix.gettimeofday ());
                last_error = None;
              });
          follow_open_log t);
      on_log = (fun ~key:_ ~job:_ _ -> ());
      on_error = (fun e -> set t (fun m -> { m with last_error = Some e }));
    }
  in
  let o = Monitor.run ~sw:t.sw ~clock cfg handlers in
  t.outcome <- Some o;
  set t (fun m -> { m with phase = Finished o })

(* Show a past deploy: its stages as they were. *)
let show_deploy t ~title (stages : Tracker.stage list) =
  set t (fun m -> { m with title; static = Some stages; phase = Finished `Deployed })

let key t ~page k =
  let m, action = Model.key !(t.model) ~page k in
  t.model := m;
  match action with
  | Model.Open_log job ->
      open_log t job;
      `Continue
  | Nothing -> `Continue
  | Quit -> `Quit

let view t = Render.view ~logs:t.logs !(t.model)
let final t = Render.final ~now:(Unix.gettimeofday ()) !(t.model)
