open Oeil

let src = Logs.Src.create "oeil.monitor" ~doc:"oeil deployment monitor"

module Log = (val Logs.src_log src)

type config = {
  instance : Instance.t;
  github : Github.t option;
  target : Tracker.target;
  interval : float;
  timeout : float;
  follow_logs : bool;
}

type outcome = [ `Deployed | `Failed of string | `Timeout ]

type handlers = {
  on_update : Tracker.t -> Tracker.event list -> unit;
  on_log : key:string -> job:string -> string -> unit;
  on_error : string -> unit;
}

(* Facts of finished jobs never change: cache them. *)
type cache = {
  facts : (string, Tracker.Facts.t) Hashtbl.t;
  includes : (string, bool) Hashtbl.t;
}

let create_cache () = { facts = Hashtbl.create 32; includes = Hashtbl.create 8 }

let fetch_facts ~clock ~handlers cfg cache (stages : Chain.stage list) =
  List.iter
    (fun (s : Chain.stage) ->
      match s.node.job with
      | None -> ()
      | Some job when Hashtbl.mem cache.facts job -> ()
      | Some job -> (
          let active = State.is_active (Svg_graph.effective_state s.node) in
          let r =
            if active then Instance.job_head cfg.instance ~clock job
            else Instance.job_log cfg.instance job
          in
          match r with
          | Error e -> handlers.on_error e
          | Ok log ->
              let f = Tracker.Facts.of_log log in
              if active || f.outcome = `Unknown then
                (* Keep it for this poll only. *)
                Hashtbl.replace cache.facts ("?" ^ job) f
              else Hashtbl.replace cache.facts job f))
    stages

let lookup cache job =
  match Hashtbl.find_opt cache.facts job with
  | Some f -> Some f
  | None -> Hashtbl.find_opt cache.facts ("?" ^ job)

let clear_transient cache =
  let tmp =
    Hashtbl.fold
      (fun k _ acc -> if String.length k > 0 && k.[0] = '?' then k :: acc else acc)
      cache.facts []
  in
  List.iter (Hashtbl.remove cache.facts) tmp

let includes ~handlers cfg cache ~newer =
  match (cfg.target.sha, cfg.github) with
  | None, _ -> Some true
  | Some _, None -> None
  | Some commit, Some gh -> (
      match Hashtbl.find_opt cache.includes newer with
      | Some b -> Some b
      | None -> (
          match Github.includes gh ~repo:cfg.target.repo ~commit ~newer with
          | Ok b ->
              Hashtbl.replace cache.includes newer b;
              Some b
          | Error e ->
              handlers.on_error e;
              None))

let query cfg = "repo=" ^ cfg.target.repo

(* One poll: fetch the graph and the facts, and step the tracker. *)
let poll ~clock ~handlers cfg cache tracker =
  match Instance.pipeline cfg.instance ~query:(query cfg) () with
  | Error e ->
      handlers.on_error e;
      None
  | Ok graph -> (
      match Chain.stages graph ~repo:cfg.target.repo ~branch:cfg.target.branch with
      | Error e ->
          handlers.on_error e;
          None
      | Ok stages ->
          clear_transient cache;
          fetch_facts ~clock ~handlers cfg cache stages;
          let tracker, events =
            Tracker.step tracker ~stages ~facts:(lookup cache)
              ~includes:(includes ~handlers cfg cache)
          in
          Some (tracker, events))

let snapshot ~clock ?(on_error = fun _ -> ()) cfg =
  let handlers =
    { on_update = (fun _ _ -> ()); on_log = (fun ~key:_ ~job:_ _ -> ()); on_error }
  in
  let cache = create_cache () in
  poll ~clock ~handlers cfg cache (Tracker.create cfg.target)

let run ~sw ~clock ?(stop = fun _ -> false) cfg handlers : outcome =
  let cache = create_cache () in
  let following = Hashtbl.create 8 in
  let follow (st : Tracker.stage) =
    match st.job with
    | Some job when cfg.follow_logs && not (Hashtbl.mem following job) ->
        Hashtbl.replace following job ();
        Eio.Fiber.fork_daemon ~sw (fun () ->
            (match
               Instance.follow_job cfg.instance job ~on_line:(fun l ->
                   handlers.on_log ~key:st.key ~job l)
             with
            | Ok () -> ()
            | Error e -> handlers.on_error e);
            `Stop_daemon)
    | _ -> ()
  in
  let deadline = Eio.Time.now clock +. cfg.timeout in
  let rec loop tracker =
    let tracker =
      match poll ~clock ~handlers cfg cache tracker with
      | None -> tracker
      | Some (tracker, events) ->
          List.iter
            (fun (st : Tracker.stage) ->
              match st.status with Tracker.Running -> follow st | _ -> ())
            (Tracker.stages tracker);
          handlers.on_update tracker events;
          tracker
    in
    match Tracker.verdict tracker with
    | Tracker.Deployed -> `Deployed
    | Tracker.Failed_at k -> `Failed k
    | Tracker.In_progress ->
        if stop tracker then `Timeout
        else if Eio.Time.now clock >= deadline then `Timeout
        else begin
          Eio.Time.sleep clock cfg.interval;
          loop tracker
        end
  in
  loop (Tracker.create cfg.target)
