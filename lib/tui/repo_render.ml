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

(* {1 Feeds} *)

(* "36 5 * * *" -> "daily 05:36 UTC"; "0 1 * * MON" -> "MON 01:00 UTC". *)
let schedule_text (f : Ci.feed) =
  match f.schedules with
  | [] -> "on push"
  | c :: rest -> (
      let more = if rest = [] then "" else Printf.sprintf " +%d" (List.length rest) in
      match String.split_on_char ' ' c.text |> List.filter (( <> ) "") with
      | [ mi; h; "*"; "*"; dow ]
        when int_of_string_opt mi <> None && int_of_string_opt h <> None ->
          let hm = Printf.sprintf "%02d:%02d" (int_of_string h) (int_of_string mi) in
          (if dow = "*" then "daily " else dow ^ " ") ^ hm ^ " UTC" ^ more
      | _ -> c.text ^ more)

let outcome_view (o : Ci.outcome) =
  match o with
  | Failed _ -> View.role `Failed (text (Ci.outcome_to_string o))
  | Opened _ | Updated _ -> View.role `Accent (text (Ci.outcome_to_string o))
  | In_progress -> View.role `Warning (text "running")
  | No_change | Succeeded -> muted (Ci.outcome_to_string o)

let outcome_mark ~live (o : Ci.outcome) =
  match o with
  | Failed _ -> View.role `Failed (text "✗")
  | In_progress -> View.role `Warning (if live then Spinner.view () else text "●")
  | _ -> View.role `Succeeded (text "✔")

(* Where the feed's latest PR is now: from the branch summaries when it is
   active, else from GitHub. *)
let pr_fate (s : RM.t) (f : Ci.feed) =
  match Ci.last_pr f with
  | None -> View.empty
  | Some n -> (
      let entry =
        List.find_map
          (fun (b : RM.branch) ->
            List.find_opt
              (fun (e : Repo_state.entry) ->
                match e.pr with Some p -> p.number = n | None -> false)
              b.entries)
          s.branches
      in
      let label = "#" ^ string_of_int n ^ " " in
      match entry with
      | Some e -> (
          match e.state with
          | Live _ -> View.role `Succeeded (text (label ^ "live"))
          | Waiting -> View.role `Warning (text (label ^ "waiting"))
          | Deploying d -> View.role `Warning (text (label ^ d))
          | Failed k -> View.role `Failed (text (label ^ "failed at " ^ k)))
      | None -> (
          match List.find_opt (fun (p : Ci.bot_pr) -> p.number = n) f.prs with
          | Some { merged = Some _; _ } -> View.role `Succeeded (text (label ^ "live"))
          | Some { closed = true; _ } -> muted (label ^ "closed")
          | Some _ -> View.role `Warning (text (label ^ "open"))
          | None -> muted label))

let strip_view ~now ~days f =
  View.concat
    (List.map
       (function
         | Ci.Pr -> View.role `Accent (text "●")
         | Changeless -> muted "○"
         | Fail -> View.role `Failed (text "✗")
         | Ok_run -> View.role `Succeeded (text "✔")
         | No_run -> muted "·")
       (Ci.strip ~days ~now f))

(* Coarse: "45m", "16h", "6d". *)
let coarse secs =
  let s = int_of_float (Float.max 0. secs) in
  if s < 3600 then Printf.sprintf "%dm" (max 1 (s / 60))
  else if s < 86400 then Printf.sprintf "%dh" (s / 3600)
  else Printf.sprintf "%dd" (s / 86400)

(* The table drops the schedule column, then half the strip, as the
   terminal narrows. *)
let feeds_table ~now ~live ~selected (s : RM.t) =
  View.fit (fun room ->
      let width = Option.value (Room.width room) ~default:120 in
      let with_schedule = width >= 120 in
      let days = if width >= 90 then 14 else 7 in
      let next (f : Ci.feed) =
        match
          List.filter_map (fun c -> Cron.next c ~now) f.schedules |> List.sort compare
        with
        | t :: _ -> muted ("in " ^ coarse (t -. now))
        | [] -> muted "on push"
      in
      let col ?(keep = true) c = if keep then [ c ] else [] in
      Table.view
        ([
           Table.column ~shrinkable:false (text "");
           Table.column ~shrinkable:false (text "");
           Table.column ~min_width:10 ~overflow:`Truncate (text "feed");
         ]
        @ col ~keep:with_schedule (Table.column ~overflow:`Truncate (text "schedule"))
        @ [
            Table.column ~shrinkable:false (text "last");
            Table.column ~min_width:9 ~overflow:`Truncate (text "outcome");
            Table.column ~min_width:9 ~overflow:`Truncate (text "its PR");
            Table.column ~shrinkable:false ~align:`Right (text "next");
            Table.column ~shrinkable:false (text (Printf.sprintf "%d days" days));
          ])
        (List.mapi
           (fun i (f : Ci.feed) ->
             let sel = i = selected in
             let name = text f.name in
             let last, outcome, m =
               match f.runs with
               | (r, o) :: _ ->
                   ( muted (coarse (now -. r.started) ^ " ago"),
                     outcome_view o,
                     outcome_mark ~live o )
               | [] -> (muted "-", View.empty, muted "·")
             in
             [
               (if sel then text "›" else text " ");
               m;
               (if sel then View.styled Style.reverse name else name);
             ]
             @ (if with_schedule then [ muted (schedule_text f) ] else [])
             @ [ last; outcome; pr_fate s f; next f; strip_view ~now ~days f ])
           s.feeds))

let body ~now ~live (m : Repo_model.t) =
  match m.snapshot with
  | None -> [ muted "  reading the pipeline, the deploy history and GitHub…" ]
  | Some s ->
      let feeds =
        match s.feeds with
        | [] -> []
        | _ ->
            [
              View.role `Header (text "feeds");
              feeds_table ~now ~live ~selected:m.selected s;
              muted "  ● opened or updated a PR  ○ no change  ✗ failed  ✔ ran  · no run";
            ]
      in
      let errors =
        List.map (fun e -> View.role `Warning (text ("  ⚠ " ^ e))) s.feed_errors
      in
      let offset = ref (List.length s.feeds) in
      feeds @ errors
      @ List.concat_map
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
        @ [ muted "↑↓ select  ⏎ timeline and logs  r refresh  q quit" ]))

let final (m : Repo_model.t) =
  let now = Unix.gettimeofday () in
  View.vcat
    ([ header ~now ~live:false { m with refreshing = false } ]
    @ body ~now ~live:false { m with selected = -1 })
