open Oeil
open Console

let text = View.text
let muted s = View.role `Muted (text s)

(* "2026-11-16 09:23.52" (UTC, as OCurrent logs it) -> seconds since epoch. *)
let time_of_since s =
  try
    Scanf.sscanf s "%4d-%2d-%2d %2d:%2d.%2d" (fun y mo d h mi se ->
        Option.map Ptime.to_float_s (Ptime.of_date_time ((y, mo, d), ((h, mi, se), 0))))
  with _ -> None

let duration secs =
  let s = int_of_float (Float.max 0. secs) in
  if s < 60 then Printf.sprintf "%ds" s
  else if s < 3600 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60)
  else Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)

(* Local time, with the date when it is not today. *)
let clock_time ~now t =
  let tm = Unix.localtime t and today = Unix.localtime now in
  if tm.tm_year = today.tm_year && tm.tm_yday = today.tm_yday then
    Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec
  else
    Printf.sprintf "%02d-%02d %02d:%02d" (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

(* "2026-11-16/094430-docker-pull-4f0aba" -> "4f0aba" *)
let job_hash job =
  match String.rindex_opt job '-' with
  | Some i -> String.sub job (i + 1) (String.length job - i - 1)
  | None -> job

let short_sha s = if String.length s > 9 then String.sub s 0 9 else s

let mark ?(live = true) (s : Tracker.status) =
  match s with
  | Done | Unchanged -> View.role `Succeeded (text "✔")
  | Failed _ -> View.role `Failed (text "✗")
  | Running -> View.role `Warning (if live then Spinner.view () else text "●")
  | Queued | Needs_confirmation -> View.role `Warning (text "○")
  | Pending -> muted "·"
  | Blocked -> muted "⊘"

let status_view (s : Tracker.status) =
  match s with
  | Done -> View.role `Succeeded (text "done")
  | Unchanged -> View.role `Succeeded (text "unchanged")
  | Failed _ -> View.role `Failed (text "failed")
  | Running -> View.role `Warning (text "running")
  | Queued -> View.role `Warning (text "queued")
  | Needs_confirmation -> View.role `Accent (text "needs confirmation")
  | Pending -> muted "pending"
  | Blocked -> muted "blocked"

let time_cell ~now (st : Tracker.stage) =
  match (st.status, Option.bind st.since time_of_since) with
  | Running, Some t0 -> View.role `Warning (text (duration (now -. t0)))
  | _, Some t0 -> muted (clock_time ~now t0)
  | _, None -> View.empty

let phase_view ?(live = true) ~now (m : Model.t) =
  match m.phase with
  | Resolving -> muted "resolving…"
  | Waiting_merge s -> View.role `Warning (text s)
  | Watching ->
      View.role `Warning
        (View.hcat ~gap:1
           [
             (if live then Spinner.view () else text "●");
             text ("in progress " ^ duration (now -. m.started));
           ])
  | Finished `Deployed -> View.role `Succeeded (text "✔ deployed")
  | Finished (`Failed k) -> View.role `Failed (text ("✗ failed at " ^ k))
  | Finished `Timeout -> View.role `Failed (text "timed out")
  | Error e -> View.role `Failed (text ("✗ " ^ e))

let header ?live ~now (m : Model.t) =
  let commit =
    match m.tracker with
    | None -> []
    | Some tr -> (
        let target = Tracker.target tr in
        match (Tracker.commit tr, target.sha) with
        | Some c, Some s when not (Tracker.sha_matches c s) ->
            [
              muted
                (Printf.sprintf "deploying %s, which includes %s" (short_sha c)
                   (short_sha s));
            ]
        | Some c, None -> [ muted ("deploying " ^ short_sha c) ]
        | _ -> [])
  in
  View.vcat
    ([ View.hcat ~gap:3 [ View.role `Header (text m.title); phase_view ?live ~now m ] ]
    @ commit)

let footer ~now ~keys (m : Model.t) =
  let poll =
    match (m.phase, m.last_poll) with
    | Watching, Some t -> Printf.sprintf "polled %s ago" (duration (now -. t))
    | _ -> ""
  in
  let err =
    match m.last_error with
    | Some e -> [ View.role `Warning (text ("⚠ " ^ e)) ]
    | None -> []
  in
  View.vcat (err @ [ View.hcat ~gap:3 [ muted keys; muted poll ] ])

let summary_table ~now ?(live = true) ?(selected = -1) (m : Model.t) =
  let rows =
    List.mapi
      (fun i (st : Tracker.stage) ->
        let sel v = if i = selected then View.styled Style.reverse v else v in
        [
          (if i = selected then text "›" else text " ");
          mark ~live st.status;
          sel (text (Chain.kind_label st.kind));
          status_view st.status;
          muted (Option.fold ~none:"" ~some:job_hash st.job);
          time_cell ~now st;
        ])
      (Model.stages m)
  in
  Table.view
    [
      Table.column ~shrinkable:false (text "");
      Table.column ~shrinkable:false (text "");
      Table.column ~min_width:16 ~overflow:`Wrap (text "stage");
      Table.column ~shrinkable:false (text "status");
      Table.column ~shrinkable:false (text "job");
      Table.column ~shrinkable:false ~align:`Right (text "time");
    ]
    rows

let summary ~now ~width (m : Model.t) =
  let body =
    match Model.stages m with
    | [] -> muted "  waiting for the pipeline…"
    | _ -> summary_table ~now ~selected:m.selected m
  in
  let failure =
    match Model.selected_stage m with
    | Some { status = Failed msg; _ } ->
        View.wrap (max 20 (width - 2)) (text msg)
        |> List.filteri (fun i _ -> i < 3)
        |> List.map (fun l -> View.role `Failed (View.hcat [ text "  "; l ]))
    | _ -> []
  in
  View.vcat
    ([ header ~now m; body ]
    @ failure
    @ [ footer ~now ~keys:"↑↓ select  ⏎ log  q quit" m ])

(* The last frame, left in the scrollback. *)
let final ~now (m : Model.t) =
  View.vcat
    [
      header ~live:false ~now m;
      (match Model.stages m with
      | [] -> View.empty
      | _ -> summary_table ~live:false ~now m);
    ]

(* A log, scrolled [offset] lines above its end, under [title]. *)
let log_panel ~title ~(log : Log_store.log option) ~offset ~height ~width =
  let visible = max 1 (height - 5) in
  let body, position =
    match log with
    | None -> ([ muted "loading…" ], "")
    | Some l ->
        let n = Dynarray.length l.lines in
        let offset = min offset (max 0 (n - visible)) in
        let last = n - offset in
        let first = max 0 (last - visible) in
        let lines =
          List.init (last - first) (fun i ->
              View.truncate width (text (Dynarray.get l.lines (first + i))))
        in
        let lines =
          match l.error with
          | Some e -> lines @ [ View.role `Failed (text e) ]
          | None -> lines
        in
        let state =
          if l.complete then "end of log"
          else if offset = 0 then "following"
          else "paused"
        in
        ( (if lines = [] then [ muted (if l.complete then "(empty log)" else "loading…") ]
           else lines),
          Printf.sprintf "lines %d–%d of %d · %s"
            (l.dropped + first + 1)
            (l.dropped + last) (l.dropped + n) state )
  in
  let pad = List.init (max 0 (visible - List.length body)) (fun _ -> text "") in
  View.vcat
    ([ title; Rule.view () ]
    @ body @ pad
    @ [
        Rule.view ();
        View.hcat ~gap:3
          [ muted "esc back  ↑↓ pgup pgdn scroll  g top  G follow"; muted position ];
      ])

let log_view ~now ~(logs : Log_store.t) ~key ~offset ~height ~width (m : Model.t) =
  let st = Model.stage m key in
  let title =
    match st with
    | Some st ->
        View.hcat ~gap:2
          [
            View.role `Header (text (Chain.kind_label st.kind));
            muted (Option.value st.job ~default:"");
            status_view st.status;
            time_cell ~now st;
          ]
    | None -> muted key
  in
  let log =
    Option.bind st (fun (st : Tracker.stage) -> Option.bind st.job (Log_store.find logs))
  in
  log_panel ~title ~log ~offset ~height ~width

let view ~(logs : Log_store.t) (m : Model.t) =
  View.fit (fun room ->
      let now = Unix.gettimeofday () in
      let width = Option.value (Room.width room) ~default:100 in
      let height = Option.value (Room.height room) ~default:24 in
      match m.mode with
      | Summary -> summary ~now ~width m
      | Log { key; offset } -> log_view ~now ~logs ~key ~offset ~height ~width m)
