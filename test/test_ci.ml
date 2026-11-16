open Oeil

let utc y mo d h mi =
  Ptime.to_float_s (Option.get (Ptime.of_date_time ((y, mo, d), ((h, mi, 0), 0))))

let cron s =
  match Cron.parse s with Some c -> c | None -> Alcotest.failf "cannot parse %S" s

let time = Alcotest.(option (float 0.))

let cron_next () =
  (* Monday 2026-11-16 10:00 UTC. *)
  let now = utc 2026 11 16 10 0 in
  Alcotest.check time "daily, later today"
    (Some (utc 2026 11 16 12 0))
    (Cron.next (cron "0 12 * * *") ~now);
  Alcotest.check time "daily, tomorrow"
    (Some (utc 2026 11 17 5 36))
    (Cron.next (cron "36 5 * * *") ~now);
  Alcotest.check time "weekly by name"
    (Some (utc 2026 11 23 1 0))
    (Cron.next (cron "0 1 * * MON") ~now);
  Alcotest.check time "every 15 min"
    (Some (utc 2026 11 16 10 15))
    (Cron.next (cron "*/15 * * * *") ~now);
  Alcotest.check time "range of days"
    (Some (utc 2026 11 17 9 0))
    (Cron.next (cron "0 9 * * TUE-THU") ~now);
  Alcotest.(check bool)
    "bad" true
    (Cron.parse "61 * * * *" = None || Cron.next (cron "61 * * * *") ~now = None)

let workflow_file () =
  let src =
    {|on:
  schedule:
    - cron: '36 5 * * *'
  workflow_dispatch:
jobs:
  scrape:
    steps:
      - uses: peter-evans/create-pull-request@v8
        with:
          branch: bot/planet
|}
  in
  Alcotest.(check (list string))
    "crons" [ "36 5 * * *" ]
    (List.map (fun (c : Cron.t) -> c.text) (Cron.of_workflow src));
  Alcotest.(check (option string)) "bot branch" (Some "bot/planet") (Ci.bot_branch src);
  let via_env =
    {|env:
  BRANCH: bot/attribution
steps:
  - uses: peter-evans/create-pull-request@v8
    with:
      branch: ${{ env.BRANCH }}
|}
  in
  Alcotest.(check (option string))
    "branch from env" (Some "bot/attribution") (Ci.bot_branch via_env);
  Alcotest.(check (option string)) "no PR" None (Ci.bot_branch "on: push\njobs: {}\n")

let run ?(conclusion = Some "success") ?(status = "completed") started finished =
  {
    Ci.id = 1;
    event = "schedule";
    status;
    conclusion;
    started;
    finished = Some finished;
    head_sha = "";
    url = "";
  }

let outcomes () =
  let t0 = utc 2026 11 15 5 37 in
  let r = run t0 (t0 +. 1180.) in
  let pr ?(commits = []) number created =
    { Ci.number; created; merged = None; closed = false; commit_dates = commits }
  in
  let check name expected prs =
    Alcotest.(check string)
      name expected
      (Ci.outcome_to_string (Ci.outcome ~produces_prs:true ~prs r))
  in
  check "opened" "opened #4015" [ pr 4015 (t0 +. 1170.) ];
  check "updated" "updated #4020" [ pr 4020 (t0 -. 86400.) ~commits:[ t0 +. 600. ] ];
  check "no change" "no change" [ pr 4014 (t0 -. 86400.) ];
  Alcotest.(check string)
    "failed" "failed"
    (Ci.outcome_to_string
       (Ci.outcome ~produces_prs:true ~prs:[] (run ~conclusion:(Some "failure") t0 t0)));
  Alcotest.(check string)
    "plain workflow" "succeeded"
    (Ci.outcome_to_string (Ci.outcome ~produces_prs:false ~prs:[] r))

let strip () =
  let now = utc 2026 11 16 12 0 in
  let day d = utc 2026 11 d 6 0 in
  let f runs =
    {
      Ci.workflow_id = 1;
      name = "planet";
      path = "";
      schedules = [];
      branch = Some "bot/planet";
      runs;
      prs = [];
    }
  in
  let cells =
    Ci.strip ~days:4 ~now
      (f
         [
           (run (day 16) (day 16), Ci.No_change);
           (run (day 15) (day 15), Ci.Opened 4015);
           (run (day 14) (day 14), Ci.Failed "failure");
           (run (day 14) (day 14), Ci.Opened 4014);
         ])
  in
  Alcotest.(check bool) "cells" true (cells = [ Ci.No_run; Fail; Pr; Changeless ])

let timeline () =
  let s = Lifecycle.step in
  let steps =
    Lifecycle.with_durations
      [
        s ~time:0. ~key:"opened" ~status:Done "opened";
        s ~time:100. ~key:"checks" ~status:Done "checks";
        s ~time:400. ~key:"merge:wait" ~status:Done ~duration:600. "review and merge";
        s ~time:1000. ~key:"live" ~status:Done "live";
      ]
  in
  Alcotest.(check (list (option (float 0.))))
    "durations"
    [ Some 100.; Some 300.; Some 600.; None ]
    (List.map (fun (s : Lifecycle.step) -> s.duration) steps);
  Alcotest.(check (option (pair (float 0.) (float 0.))))
    "summary"
    (Some (1000., 600.))
    (Lifecycle.summary steps)

let health_run () =
  (* check.ci names its latest run in its log links, as ocaml.org's scraper
     reads it. *)
  let page =
    {|<a href="/diff">Differences</a><a href="/log/notarun/x/">x</a>
<a href="/log/1794302746-78c202b54f936f55f02e3237df71f8b4d2405807/good/5.4/dune.3.20.0">dune</a>|}
  in
  match Oeil_net.Upstreams.run_of_page page with
  | None -> Alcotest.fail "no run found"
  | Some r ->
      Alcotest.(check string)
        "id" "1794302746-78c202b54f936f55f02e3237df71f8b4d2405807" r.run_id;
      Alcotest.(check string)
        "opam-repository commit" "78c202b54f936f55f02e3237df71f8b4d2405807" r.opam_commit;
      Alcotest.(check (option (float 0.))) "time" (Some 1794302746.) r.run_time

let () =
  Alcotest.run "oeil-ci"
    [
      ( "cron",
        [
          Alcotest.test_case "next run" `Quick cron_next;
          Alcotest.test_case "workflow file" `Quick workflow_file;
        ] );
      ( "runs",
        [
          Alcotest.test_case "outcomes" `Quick outcomes;
          Alcotest.test_case "14-day strip" `Quick strip;
        ] );
      ("lifecycle", [ Alcotest.test_case "durations and summary" `Quick timeline ]);
      ("upstreams", [ Alcotest.test_case "check.ci run" `Quick health_run ]);
    ]
