open Oeil
open Console
module RM = Oeil_net.Repo_monitor

let text = View.text
let muted = Render.muted

(* "2026-11-16T09:23:48Z" -> seconds since the epoch. *)
let time_of_iso s =
  try
    Scanf.sscanf s "%4d-%2d-%2dT%2d:%2d:%2d" (fun y mo d h mi se ->
        Option.map Ptime.to_float_s (Ptime.of_date_time ((y, mo, d), ((h, mi, se), 0))))
  with _ -> None

(* Coarse: "now", "17m ago", "3h ago", "2d ago". *)
let ago ~now t =
  let s = int_of_float (Float.max 0. (now -. t)) in
  if s < 60 then "now"
  else if s < 3600 then Printf.sprintf "%dm ago" (s / 60)
  else if s < 86400 then Printf.sprintf "%dh ago" (s / 3600)
  else Printf.sprintf "%dd ago" (s / 86400)

let deploy_time job = Oeil_net.Jobs.seconds_of_key (Oeil_net.Jobs.time_key job)

let mark (s : Repo_state.state) ~live =
  match s with
  | Live _ -> View.role `Succeeded (text "✔")
  | Failed _ -> View.role `Failed (text "✗")
  | Deploying _ -> View.role `Warning (if live then Spinner.view () else text "●")
  | Waiting -> View.role `Warning (text "○")

let state_view (s : Repo_state.state) =
  match s with
  | Live _ -> View.role `Succeeded (text "live")
  | Failed k -> View.role `Failed (text ("failed at " ^ k))
  | Deploying d -> View.role `Warning (text d)
  | Waiting -> View.role `Warning (text "waiting for the deployer")

let when_view ~now (e : Repo_state.entry) =
  match e.state with
  | Live (Some at) -> (
      match deploy_time at with
      | Some t -> muted (Render.clock_time ~now t)
      | None -> View.empty)
  | Live None -> View.empty
  | Waiting | Deploying _ | Failed _ -> (
      let merged =
        match e.pr with
        | Some p -> time_of_iso p.merged_at
        | None -> time_of_iso e.commit.date
      in
      match merged with Some t -> muted ("merged " ^ ago ~now t) | None -> View.empty)

let short = Render.short_sha

let branch_line ~now ~live (b : RM.branch) =
  let live_part =
    match (b.live, b.live_since) with
    | Some c, Some at ->
        let when_ =
          match deploy_time at with
          | Some t -> " since " ^ Render.clock_time ~now t
          | None -> ""
        in
        View.role `Succeeded (text ("live " ^ short c ^ when_))
    | Some c, None -> View.role `Succeeded (text ("live " ^ short c))
    | None, _ -> muted "no successful deploy found recently"
  in
  let build_part =
    match b.tracker with
    | None -> []
    | Some tr -> (
        match (Tracker.verdict tr, Tracker.commit tr) with
        | Tracker.Deployed, _ -> []
        | Tracker.In_progress, Some c
          when Option.fold ~none:true
                 ~some:(fun l -> not (Tracker.sha_matches l c))
                 b.live ->
            [
              View.role `Warning
                (View.hcat ~gap:1
                   [
                     (if live then Spinner.view () else text "●");
                     text ("deploying " ^ short c ^ ": " ^ RM.furthest tr);
                   ]);
            ]
        | Tracker.Failed_at k, Some c ->
            [ View.role `Failed (text ("✗ latest build " ^ short c ^ " failed at " ^ k)) ]
        | _ -> [])
  in
  View.hcat ~gap:3 ([ View.role `Header (text b.branch); live_part ] @ build_part)

let table ~now ~live ~selected ~offset (b : RM.branch) =
  let rows =
    List.mapi
      (fun i (e : Repo_state.entry) ->
        let sel = offset + i = selected in
        let hl v = if sel then View.styled Style.reverse v else v in
        let number, title =
          match e.pr with
          | Some p -> ("#" ^ string_of_int p.number, p.title)
          | None -> (short e.commit.sha, e.commit.message)
        in
        [
          (if sel then text "›" else text " ");
          mark ~live e.state;
          hl (text number);
          text title;
          state_view e.state;
          when_view ~now e;
        ])
      b.entries
  in
  Table.view
    [
      Table.column ~shrinkable:false (text "");
      Table.column ~shrinkable:false (text "");
      Table.column ~shrinkable:false (text "PR");
      Table.column ~min_width:16 ~overflow:`Truncate (text "title");
      Table.column ~max_width:36 ~overflow:`Truncate (text "state");
      Table.column ~shrinkable:false ~align:`Right (text "when");
    ]
    rows

let body ~now ~live (m : Repo_model.t) =
  match m.snapshot with
  | None -> [ muted "  reading the pipeline, the deploy history and GitHub…" ]
  | Some s ->
      let offset = ref 0 in
      List.concat_map
        (fun (b : RM.branch) ->
          let t =
            if b.entries = [] then
              [ muted "  no PR merged since the last deploy, none deployed recently" ]
            else [ table ~now ~live ~selected:m.selected ~offset:!offset b ]
          in
          offset := !offset + List.length b.entries;
          [ branch_line ~now ~live b ]
          @ t
          @ List.map (fun e -> View.role `Warning (text ("  ⚠ " ^ e))) b.errors)
        s.branches

let header ~now ~live (m : Repo_model.t) =
  let status =
    if m.refreshing then
      View.role `Warning
        (View.hcat ~gap:1
           [ (if live then Spinner.view () else text "●"); text "refreshing" ])
    else
      match m.snapshot with
      | Some s -> muted ("refreshed " ^ ago ~now s.at)
      | None -> View.empty
  in
  View.hcat ~gap:3 [ View.role `Header (text m.repo); status ]

let view (m : Repo_model.t) =
  View.fit (fun _room ->
      let now = Unix.gettimeofday () in
      View.vcat
        ([ header ~now ~live:true m ]
        @ body ~now ~live:true m
        @ (match m.last_error with
          | Some e -> [ View.role `Warning (text ("⚠ " ^ e)) ]
          | None -> [])
        @ [ muted "↑↓ select  ⏎ stages and logs  r refresh  q quit" ]))

let final (m : Repo_model.t) =
  let now = Unix.gettimeofday () in
  View.vcat
    ([ header ~now ~live:false { m with refreshing = false } ]
    @ body ~now ~live:false { m with selected = -1 })
