open Oeil
open Oeil_tui

let read s = Keys.read (Eio.Buf_read.of_string s)

let keys () =
  let check name expected s = Alcotest.(check bool) name true (read s = expected) in
  check "up" Keys.Up "\027[A";
  check "down (app mode)" Keys.Down "\027OB";
  check "page up" Keys.Page_up "\027[5~";
  check "page down" Keys.Page_down "\027[6~";
  check "home" Keys.Home "\027[H";
  check "end" Keys.End "\027[4~";
  check "ctrl-up is up" Keys.Up "\027[1;5A";
  check "escape alone" Keys.Escape "\027";
  check "enter" Keys.Enter "\r";
  check "q" (Keys.Char 'q') "q"

(* A tracker with the 2026-11-16 ocaml.org deploy, from the fixtures. *)
let tracker () =
  let read f =
    In_channel.with_open_bin (Filename.concat "fixtures" f) In_channel.input_all
  in
  let g = Result.get_ok (Svg_graph.parse (read "ocaml.org.svg")) in
  let stages = Result.get_ok (Chain.stages g ~repo:"ocaml/ocaml.org" ~branch:"main") in
  let facts job =
    let f = "job-" ^ Filename.basename job ^ ".html" in
    if Sys.file_exists (Filename.concat "fixtures" f) then
      Some (Tracker.Facts.of_log (Job_log.of_html (read f)))
    else None
  in
  let t =
    Tracker.create { repo = "ocaml/ocaml.org"; branch = "main"; sha = Some "ea9bd21" }
  in
  fst (Tracker.step t ~stages ~facts ~includes:(fun ~newer:_ -> Some false))

let navigation () =
  let m = Model.with_tracker (Model.v ~title:"t" ~now:0.) (tracker ()) in
  let press m k = fst (Model.key m ~page:10 k) in
  let m = press (press m Keys.Down) Keys.Down in
  Alcotest.(check int) "selected" 2 m.selected;
  let m = press m Keys.Up in
  Alcotest.(check int) "up" 1 m.selected;
  let m = press (press m Keys.End) Keys.Down in
  Alcotest.(check int) "clamped at the end" 6 m.selected;
  let m = press (press m Keys.Home) Keys.Up in
  Alcotest.(check int) "clamped at the start" 0 m.selected;
  (* Enter opens the log of the selected stage's job. *)
  let m, action = Model.key m ~page:10 Keys.Enter in
  (match action with
  | Model.Open_log j ->
      Alcotest.(check string) "job" "2026-11-16/082329-ocluster-build-13f132" j
  | _ -> Alcotest.fail "expected Open_log");
  (match m.mode with
  | Log { key; offset = 0 } -> Alcotest.(check string) "log of" "build:linux-x86_64" key
  | _ -> Alcotest.fail "expected log mode");
  (* Scrolling, then back to the summary with the selection kept. *)
  let m = press (press m Keys.Page_up) Keys.Up in
  (match m.mode with
  | Log { offset; _ } -> Alcotest.(check int) "scrolled" 11 offset
  | _ -> Alcotest.fail "expected log mode");
  let m = press m Keys.End in
  (match m.mode with
  | Log { offset; _ } -> Alcotest.(check int) "follow" 0 offset
  | _ -> Alcotest.fail "expected log mode");
  let m = press m Keys.Escape in
  Alcotest.(check bool) "summary" true (m.mode = Summary);
  Alcotest.(check int) "selection kept" 0 m.selected;
  let m, action = Model.key m ~page:10 (Keys.Char 'q') in
  Alcotest.(check bool) "quit" true (m.quit && action = Model.Quit)

let no_job () =
  (* Enter on a stage without a job of ours does nothing. *)
  let t =
    Tracker.create { repo = "ocaml/ocaml.org"; branch = "main"; sha = Some "0123456789" }
  in
  let m = Model.with_tracker (Model.v ~title:"t" ~now:0.) t in
  let m, action = Model.key m ~page:10 Keys.Enter in
  Alcotest.(check bool) "nothing" true (action = Model.Nothing && m.mode = Summary)

let time () =
  Alcotest.(check (option (float 0.)))
    "since" (Some 1794820992.)
    (Render.time_of_since "2026-11-16 09:23.12");
  Alcotest.(check string) "duration" "12m03s" (Render.duration 723.);
  Alcotest.(check string) "hours" "1h02m" (Render.duration 3725.);
  Alcotest.(check string)
    "job hash" "4f0aba"
    (Render.job_hash "2026-11-16/094430-docker-pull-4f0aba")

let () =
  Alcotest.run "oeil-tui"
    [
      ("keys", [ Alcotest.test_case "decode" `Quick keys ]);
      ( "model",
        [
          Alcotest.test_case "navigation" `Quick navigation;
          Alcotest.test_case "no job" `Quick no_job;
        ] );
      ("render", [ Alcotest.test_case "time" `Quick time ]);
    ]
