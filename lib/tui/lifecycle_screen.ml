(* The life of one change: what started it, its PR, checks, merge, the
   deployer's stages and when it went live. Enter on a step shows its log. *)
open Oeil
open Oeil_net
open Console
module L = Lifecycle

type mode = Steps | Log of { key : string; offset : int }

type t = {
  mutable title : string;
  mutable status : View.t;  (** right of the title *)
  mutable github : L.step list;
  mutable deploy : L.step list;
  mutable live : L.step option;
  mutable note : string option;  (** shown under the title *)
  mutable selected : int;
  mutable mode : mode;
  logs : Log_store.t;
  sw : Eio.Switch.t;
  dirty : unit -> unit;
  repo : string;
  gh : Github.t;
  inst : Instance.t;
}

let create ~sw ~dirty ~repo ~gh ~inst ~title =
  {
    title;
    status = Render.muted "loading…";
    github = [];
    deploy = [];
    live = None;
    note = None;
    selected = 0;
    mode = Steps;
    logs = Log_store.create ();
    sw;
    dirty;
    repo;
    gh;
    inst;
  }

let steps t =
  let deploy_header =
    match t.deploy with
    | [] -> []
    | first :: _ ->
        let status =
          if List.exists (fun (s : L.step) -> s.status = L.Failed) t.deploy then L.Failed
          else if List.for_all (fun (s : L.step) -> s.status = L.Done) t.deploy then
            L.Done
          else if List.exists (fun (s : L.step) -> s.status = L.Running) t.deploy then
            L.Running
          else L.Pending
        in
        [
          L.step ?time:first.time ~key:"deploy" ~status
            ~detail:(Option.value (Uri.host (Instance.base t.inst)) ~default:"")
            "deploy";
        ]
  in
  L.with_durations (t.github @ deploy_header @ t.deploy @ Option.to_list t.live)

let log_key = function
  | L.Actions_job j -> Printf.sprintf "gh:%d" j
  | L.Deployer_job j -> j
  | L.Ocurrent_job { base; job } -> base ^ "/" ^ job

(* Follow an OCurrent job's log while it runs. *)
let follow t inst job (l : Log_store.log) =
  match
    Instance.follow_job inst job ~on_line:(fun line ->
        Log_store.push l line;
        t.dirty ())
  with
  | Ok () -> l.complete <- true
  | Error e -> l.error <- Some e

(* Fetch a step's log once: an Actions job log at once, an OCurrent job's
   (the deployer's, or another service's) followed while it runs. *)
let open_log t (log : L.log) =
  let key = log_key log in
  match Log_store.find t.logs key with
  | Some _ -> ()
  | None ->
      let l = Log_store.add t.logs key in
      Eio.Fiber.fork_daemon ~sw:t.sw (fun () ->
          (match log with
          | L.Actions_job job -> (
              match Actions.job_log t.gh ~repo:t.repo ~job with
              | Ok lines ->
                  List.iter (Log_store.push l) lines;
                  l.complete <- true
              | Error e -> l.error <- Some e)
          | L.Deployer_job job -> follow t t.inst job l
          | L.Ocurrent_job { base; job } ->
              follow t (Instance.with_base t.inst base) job l);
          t.dirty ();
          `Stop_daemon)

(* Steps from elsewhere (an upstream), shown as they are. *)
let show t ~status ?note steps =
  t.github <- steps;
  t.status <- status;
  t.note <- note;
  t.dirty ()

let key t ~page (k : Keys.t) =
  let rows = steps t in
  let n = List.length rows in
  match t.mode with
  | Steps -> (
      let select i = t.selected <- max 0 (min (n - 1) i) in
      match k with
      | Up | Char 'k' ->
          select (t.selected - 1);
          `Continue
      | Down | Char 'j' ->
          select (t.selected + 1);
          `Continue
      | Home | Char 'g' ->
          select 0;
          `Continue
      | End | Char 'G' ->
          select (n - 1);
          `Continue
      | Enter | Right | Char 'l' -> (
          match List.nth_opt rows t.selected with
          | Some { log = Some log; key; _ } ->
              t.mode <- Log { key; offset = 0 };
              open_log t log;
              `Continue
          | _ -> `Continue)
      | Char ('q' | 'Q') | Escape | Left | Backspace -> `Quit
      | _ -> `Continue)
  | Log { key; offset } -> (
      let scroll o = t.mode <- Log { key; offset = max 0 o } in
      match k with
      | Up | Char 'k' ->
          scroll (offset + 1);
          `Continue
      | Down | Char 'j' ->
          scroll (offset - 1);
          `Continue
      | Page_up | Char 'b' ->
          scroll (offset + page);
          `Continue
      | Page_down | Char ' ' ->
          scroll (offset - page);
          `Continue
      | Home | Char 'g' ->
          scroll max_int;
          `Continue
      | End | Char 'G' | Char 'f' ->
          scroll 0;
          `Continue
      | Escape | Left | Backspace | Char ('q' | 'Q' | 'h') ->
          t.mode <- Steps;
          `Continue
      | _ -> `Continue)

(* {1 Rendering} *)

let text = View.text
let muted = Render.muted

let mark ~live (s : L.status) =
  match s with
  | Done -> View.role `Succeeded (text "✔")
  | Failed -> View.role `Failed (text "✗")
  | Running -> View.role `Warning (if live then Spinner.view () else text "●")
  | Waiting -> View.role `Warning (text "○")
  | Pending -> muted "·"

let table ~now ~live ~selected rows =
  Table.view
    [
      Table.column ~shrinkable:false (text "");
      Table.column ~shrinkable:false (text "");
      Table.column ~shrinkable:false ~align:`Right (text "at");
      Table.column ~min_width:14 (text "step");
      Table.column ~overflow:`Truncate (text "");
      Table.column ~shrinkable:false ~align:`Right (text "took");
    ]
    (List.mapi
       (fun i (s : L.step) ->
         let sel = i = selected in
         let label = (if s.depth > 0 then "  " else "") ^ s.label in
         let label = if s.depth = 0 then View.role `Header (text label) else text label in
         [
           (if sel then text "›" else text " ");
           mark ~live s.status;
           muted (match s.time with Some t -> Render.clock_time ~now t | None -> "");
           (if sel then View.styled Style.reverse label else label);
           (match s.status with
           | Failed -> View.role `Failed (text s.detail)
           | _ -> muted s.detail);
           (match (s.duration, s.status) with
           | Some d, Running -> View.role `Warning (text (Render.duration d))
           | Some d, _ -> muted (Render.duration d)
           | None, _ -> View.empty);
         ])
       rows)

let summary_line rows =
  match L.summary rows with
  | Some (total, waiting) when total > 0. && waiting > 0. ->
      muted
        (Printf.sprintf
           "%s from the first step to the last · %s (%.0f%%) waiting for review and merge"
           (Render.duration total) (Render.duration waiting)
           (100. *. waiting /. total))
  | Some (total, _) when total > 0. ->
      muted (Printf.sprintf "%s from the first step to the last" (Render.duration total))
  | _ -> View.empty

let header t = View.hcat ~gap:3 [ View.role `Header (text t.title); t.status ]

let view t =
  View.fit (fun room ->
      let now = Unix.gettimeofday () in
      let width = Option.value (Room.width room) ~default:100 in
      let height = Option.value (Room.height room) ~default:24 in
      let rows = steps t in
      match t.mode with
      | Steps ->
          View.vcat
            ([ header t ]
            @ (match t.note with Some n -> [ muted n ] | None -> [])
            @ [
                (if rows = [] then muted "  reading GitHub and the deployer…"
                 else table ~now ~live:true ~selected:t.selected rows);
                summary_line rows;
                muted
                  (match List.nth_opt rows t.selected with
                  | Some { log = None; _ } ->
                      "↑↓ select  (no log for this step)  esc back"
                  | _ -> "↑↓ select  ⏎ log  esc back");
              ])
      | Log { key; offset } ->
          let step = List.find_opt (fun (s : L.step) -> s.key = key) rows in
          let title =
            match step with
            | Some s ->
                View.hcat ~gap:2
                  [
                    View.role `Header (text s.label);
                    mark ~live:true s.status;
                    muted s.detail;
                  ]
            | None -> muted key
          in
          let log =
            Option.bind step (fun (s : L.step) ->
                Option.bind s.log (fun l -> Log_store.find t.logs (log_key l)))
          in
          Render.log_panel ~title ~log ~offset ~height ~width)

let final t =
  let now = Unix.gettimeofday () in
  let rows = steps t in
  View.vcat [ header t; table ~now ~live:false ~selected:(-1) rows; summary_line rows ]

(* {1 Filling it} *)

let time_of_since = Render.time_of_since

let set_deploy t ~now stages =
  t.deploy <- L.of_stages ~now stages ~time_of_since;
  t.dirty ()

let live_step ~at =
  L.step
    ?time:(Jobs.seconds_of_key (Jobs.time_key at))
    ~key:"live" ~status:L.Done ~log:(L.Deployer_job at) "live"

(* A PR: its GitHub steps, then its deploy. [deploy] is [`Past (stages,
   deploy)] for a live PR, [`Follow target] for a merged one not live yet,
   [`None] for an open PR. *)
let fill_pr t ~clock ~jobs ~(feeds : Ci.feed list) ~number ~deploy =
  let rec github_loop () =
    (match Lifecycle_data.pr t.gh ~repo:t.repo ~feeds ~number with
    | Ok (p, steps) ->
        t.github <- steps;
        if p.p_merged = None then
          t.status <-
            (if p.p_state = "closed" then View.role `Failed (text "closed")
             else View.role `Warning (text "open"))
    | Error e -> t.note <- Some ("⚠ " ^ e));
    t.dirty ();
    (* Until merged, checks and review move: look again now and then. *)
    match deploy with
    | `None ->
        Eio.Time.sleep clock 60.;
        github_loop ()
    | _ -> ()
  in
  github_loop ();
  match deploy with
  | `None -> ()
  | `Past (stages, (d : Repo_state.deploy)) ->
      set_deploy t ~now:(Unix.gettimeofday ()) (Deploys.stages_of jobs ~stages d);
      t.live <- Some (live_step ~at:d.at);
      t.status <- View.role `Succeeded (text "✔ live");
      t.dirty ()
  | `Follow (target : Tracker.target) ->
      t.status <- View.role `Warning (text "deploying");
      let cfg =
        {
          Monitor.instance = t.inst;
          github = Some t.gh;
          target;
          interval = 10.;
          timeout = 86400.;
          follow_logs = false;
        }
      in
      let handlers =
        {
          Monitor.on_update =
            (fun tr _ ->
              set_deploy t ~now:(Unix.gettimeofday ()) (Tracker.stages tr);
              (match (Tracker.commit tr, target.sha) with
              | Some c, Some s when not (Tracker.sha_matches c s) ->
                  t.note <-
                    Some
                      (Printf.sprintf "deployed with %s, which includes it"
                         (Render.short_sha c))
              | _ -> ());
              t.dirty ());
          on_log = (fun ~key:_ ~job:_ _ -> ());
          on_error =
            (fun e ->
              t.note <- Some ("⚠ " ^ e);
              t.dirty ());
        }
      in
      (match Monitor.run ~sw:t.sw ~clock cfg handlers with
      | `Deployed ->
          t.status <- View.role `Succeeded (text "✔ live");
          t.live <-
            Some (L.step ~time:(Unix.gettimeofday ()) ~key:"live" ~status:L.Done "live")
      | `Failed k -> t.status <- View.role `Failed (text ("✗ failed at " ^ k))
      | `Timeout -> t.status <- View.role `Failed (text "timed out"));
      t.dirty ()

(* A feed's latest run, when it opened no PR. *)
let fill_run t ~(feed : Ci.feed) run =
  t.github <- Lifecycle_data.of_run t.gh ~repo:t.repo ~feed:feed.name run;
  t.status <-
    (match snd run with
    | Ci.Failed c -> View.role `Failed (text ("✗ " ^ c))
    | In_progress -> View.role `Warning (text "running")
    | o -> View.role `Succeeded (text ("✔ " ^ Ci.outcome_to_string o)));
  t.dirty ()
