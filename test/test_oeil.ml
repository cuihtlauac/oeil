open Oeil

let read f = In_channel.with_open_bin (Filename.concat "fixtures" f) In_channel.input_all
let graph f = match Svg_graph.parse (read f) with Ok g -> g | Error e -> Alcotest.fail e
let state = Alcotest.testable State.pp State.equal

let find_label g l =
  match
    List.find_opt (fun (n : Svg_graph.node) -> Svg_graph.label n = l) g.Svg_graph.nodes
  with
  | Some n -> n
  | None -> Alcotest.failf "no node labelled %S" l

(* --- Svg_graph --- *)

let test_top () =
  let g = graph "top.svg" in
  Alcotest.(check int) "nodes" 47 (List.length g.nodes);
  Alcotest.(check int) "edges" 37 (List.length g.edges);
  let peertube = find_label g "pull chocobozzz/peertube:production-bookworm" in
  Alcotest.check state "failed" State.Failed peertube.state;
  Alcotest.(check (option string))
    "job" (Some "2026-11-10/075829-docker-pull-4aed24") peertube.job;
  Alcotest.(check bool) "tooltip" true (Option.is_some peertube.tooltip);
  let svc = find_label g "docker-service | infra_peertube" in
  Alcotest.check state "blocked" State.Blocked svc.state;
  Alcotest.(check (option string)) "no tooltip" None svc.tooltip;
  Alcotest.(check (list string))
    "edge" [ peertube.id ]
    (Svg_graph.preds g svc.id |> List.map (fun (n : Svg_graph.node) -> n.id));
  let expands = List.filter_map (fun (n : Svg_graph.node) -> n.expand) g.nodes in
  Alcotest.(check bool) "expand link" true (List.mem "repo=ocaml/ocaml.org" expands)

let test_gradient () =
  let svg =
    {|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph">
<g id="node1" class="node"><title>n1</title>
<g id="a_node1"><a xlink:href="/job/2026-11-16/090000-docker-service-abc" xlink:title=" ">
<defs><linearGradient id="l_0" gradientUnits="userSpaceOnUse" x1="0" y1="0" x2="1" y2="0">
<stop offset="0" style="stop-color:#ffa500;stop-opacity:1.0;"/>
<stop offset="1" style="stop-color:#90ee90;stop-opacity:1.0;"/>
</linearGradient></defs>
<polygon fill="url(#l_0)" stroke="transparent" points="0,0 1,1"/>
<text>docker&#45;service</text><text>infra_www1 &#x2026;</text>
</a></g></g>
</g></svg>|}
  in
  match Svg_graph.parse svg with
  | Error e -> Alcotest.fail e
  | Ok g ->
      let n = List.hd g.nodes in
      Alcotest.(check (list string)) "label" [ "docker-service"; "infra_www1" ] n.label;
      Alcotest.check state "value" State.Ok n.state;
      Alcotest.(check (option state)) "update" (Some State.Running) n.update;
      Alcotest.check state "effective" State.Running (Svg_graph.effective_state n)

(* --- Chain --- *)

let test_chain () =
  let g = graph "ocaml.org.svg" in
  Alcotest.(check (list string))
    "branches" [ "staging"; "main" ]
    (Chain.branches g ~repo:"ocaml/ocaml.org");
  match Chain.stages g ~repo:"ocaml/ocaml.org" ~branch:"main" with
  | Error e -> Alcotest.fail e
  | Ok stages ->
      Alcotest.(check (list string))
        "keys"
        [
          "build:linux-x86_64";
          "push:ocurrent/v3.ocaml.org-server:live";
          "pull:infra_www2";
          "pull:infra_www1";
          "service:infra_www2";
          "service:infra_www1";
          "notify";
        ]
        (List.map (fun (s : Chain.stage) -> s.key) stages);
      let build = List.hd stages in
      Alcotest.(check (option string))
        "build job" (Some "2026-11-16/082329-ocluster-build-13f132") build.node.job

let test_chain_missing () =
  let g = graph "ocaml.org.svg" in
  match Chain.stages g ~repo:"ocaml/ocaml.org" ~branch:"nope" with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error _ -> ()

(* --- Job_log --- *)

let sha_d1 = "sha256:67a237869305052eb4f29819fcd3fafe6adb8276392e949603e84862d01537e7"
let sha_d2 = "sha256:cdab3e4018ce65a0ae67d7a9c9e969fe184f5c4612122f5db45dfbdd976af638"

let test_build_log () =
  let log = Job_log.of_html (read "job-082329-ocluster-build-13f132.html") in
  Alcotest.(check (option (pair string string)))
    "commit"
    (Some ("refs/heads/main", "ea9bd215503464270bcc090cab3422bd337f92cd"))
    (Job_log.commit log);
  Alcotest.(check (option string)) "digest" (Some sha_d1) (Job_log.build_digest log);
  Alcotest.(check bool) "succeeded" true (Job_log.outcome log = `Succeeded);
  Alcotest.(check (option string))
    "timestamp" (Some "2026-11-16 08:23.29")
    (Job_log.timestamp (List.hd log))

let test_manifest_log () =
  let page = read "job-071846-docker-push-manifest-ac5556.html" in
  let log = Job_log.of_html page in
  Alcotest.(check (list string)) "inputs" [ sha_d1 ] (Job_log.input_digests log);
  Alcotest.(check (option string)) "output" (Some sha_d2) (Job_log.output_digest log);
  Alcotest.(check (option string))
    "history head" (Some "2026-11-16/065646-docker-push-manifest-5a7e14")
    (List.nth_opt (Job_log.history page) 0)

let test_service_log () =
  let log = Job_log.of_html (read "job-071954-docker-service-f92f02.html") in
  Alcotest.(check (list string)) "inputs" [ sha_d2 ] (Job_log.input_digests log);
  let log = Job_log.of_html (read "job-071910-docker-pull-53b9b3.html") in
  Alcotest.(check (list string)) "pull inputs" [ sha_d2 ] (Job_log.input_digests log)

let test_failed_log () =
  let log = Job_log.of_html (read "job-075829-docker-pull-4aed24.html") in
  match Job_log.outcome log with `Failed _ -> () | _ -> Alcotest.fail "expected failure"

let test_stream () =
  let page = read "job-071846-docker-push-manifest-ac5556.html" in
  let whole = Job_log.of_html page in
  (* Feed in awkward chunk sizes, splitting "<pre>" and lines. *)
  List.iter
    (fun size ->
      let st = Job_log.Stream.create () in
      let acc = ref [] in
      let n = String.length page in
      let rec go i =
        if i < n then begin
          let len = min size (n - i) in
          acc := List.rev_append (Job_log.Stream.feed st (String.sub page i len)) !acc;
          go (i + len)
        end
      in
      go 0;
      acc := List.rev_append (Job_log.Stream.finish st) !acc;
      Alcotest.(check (list string))
        (Printf.sprintf "chunks of %d" size)
        whole (List.rev !acc);
      Alcotest.(check bool) "finished" true (Job_log.Stream.finished st))
    [ 1; 3; 7; 64; 100_000 ]

(* --- Tracker --- *)

let fixture_facts job =
  let file = "job-" ^ Filename.basename job ^ ".html" in
  if Sys.file_exists (Filename.concat "fixtures" file) then
    Some (Tracker.Facts.of_log (Job_log.of_html (read file)))
  else None

let main_stages () =
  let g = graph "ocaml.org.svg" in
  match Chain.stages g ~repo:"ocaml/ocaml.org" ~branch:"main" with
  | Ok s -> s
  | Error e -> Alcotest.fail e

let target sha = { Tracker.repo = "ocaml/ocaml.org"; branch = "main"; sha }
let no_includes ~newer:_ = Some false

let statuses t =
  List.map
    (fun (s : Tracker.stage) -> (s.key, Tracker.status_to_string s.status))
    (Tracker.stages t)

let verdict =
  Alcotest.testable
    (fun ppf -> function
      | Tracker.In_progress -> Format.pp_print_string ppf "in-progress"
      | Deployed -> Format.pp_print_string ppf "deployed"
      | Failed_at k -> Format.fprintf ppf "failed at %s" k)
    ( = )

let test_already_deployed () =
  let t = Tracker.create (target (Some "ea9bd2155")) in
  let t, events =
    Tracker.step t ~stages:(main_stages ()) ~facts:fixture_facts ~includes:no_includes
  in
  Alcotest.(check (list (pair string string)))
    "statuses"
    [
      ("build:linux-x86_64", "done");
      ("push:ocurrent/v3.ocaml.org-server:live", "unchanged");
      ("pull:infra_www2", "unchanged");
      ("pull:infra_www1", "unchanged");
      ("service:infra_www2", "unchanged");
      ("service:infra_www1", "unchanged");
      ("notify", "done");
    ]
    (statuses t);
  Alcotest.check verdict "verdict" Tracker.Deployed (Tracker.verdict t);
  Alcotest.(check bool)
    "finished event" true
    (List.mem (Tracker.Finished Deployed) events);
  Alcotest.(check bool)
    "building event" true
    (List.mem (Tracker.Building "ea9bd215503464270bcc090cab3422bd337f92cd") events);
  (* A second identical poll changes nothing. *)
  let _, events =
    Tracker.step t ~stages:(main_stages ()) ~facts:fixture_facts ~includes:no_includes
  in
  Alcotest.(check int) "no events" 0 (List.length events)

let test_not_built_yet () =
  (* A commit newer than what was built: nothing is ours yet. *)
  let t = Tracker.create (target (Some "0123456789abcdef")) in
  let t, _ =
    Tracker.step t ~stages:(main_stages ()) ~facts:fixture_facts ~includes:no_includes
  in
  Alcotest.(check bool)
    "all pending" true
    (List.for_all
       (fun (s : Tracker.stage) -> s.status = Tracker.Pending)
       (Tracker.stages t));
  Alcotest.check verdict "verdict" Tracker.In_progress (Tracker.verdict t)

let test_superseded () =
  let t = Tracker.create (target (Some "0123456789abcdef")) in
  let includes ~newer = Some (Tracker.sha_matches newer "ea9bd2155") in
  let t, events =
    Tracker.step t ~stages:(main_stages ()) ~facts:fixture_facts ~includes
  in
  Alcotest.(check bool)
    "superseded" true
    (List.mem (Tracker.Superseded "ea9bd215503464270bcc090cab3422bd337f92cd") events);
  Alcotest.check verdict "verdict" Tracker.Deployed (Tracker.verdict t)

(* Rewrite the main chain as if a new deploy of [ea9bd21] were under way:
   new build at 09:00, and service [infra_www1] updating. *)
let d1 = "sha256:67a237869305052eb4f29819fcd3fafe6adb8276392e949603e84862d01537e7"
let d2 = "sha256:cdab3e4018ce65a0ae67d7a9c9e969fe184f5c4612122f5db45dfbdd976af638"

let synthetic_facts ?(push_outcome = `Succeeded) () job =
  let mk ?commit ?output ?(inputs = []) ?(outcome = `Succeeded) () =
    Some
      {
        Tracker.Facts.commit;
        output;
        inputs;
        outcome;
        started = Some "2026-11-16 09:00.00";
        tail = [];
      }
  in
  match Filename.basename job with
  | "090000-ocluster-build-new" ->
      mk ~commit:"ea9bd215503464270bcc090cab3422bd337f92cd" ~output:d1 ()
  | "090200-docker-push-manifest-new" ->
      mk ~inputs:[ d1 ] ~output:d2 ~outcome:push_outcome ()
  | "090300-docker-pull-new" -> mk ~inputs:[ d2 ] ()
  | "090400-docker-service-new1" -> mk ~inputs:[ d2 ] ~outcome:`Unknown ()
  | _ -> fixture_facts job

let rewrite stages f =
  List.map (fun (s : Chain.stage) -> { s with node = f s.key s.node }) stages

let test_in_progress () =
  let stages =
    rewrite (main_stages ()) (fun key (n : Svg_graph.node) ->
        match key with
        | "build:linux-x86_64" ->
            { n with job = Some "2026-11-16/090000-ocluster-build-new" }
        | "push:ocurrent/v3.ocaml.org-server:live" ->
            { n with job = Some "2026-11-16/090200-docker-push-manifest-new" }
        | "pull:infra_www1" | "pull:infra_www2" ->
            { n with job = Some "2026-11-16/090300-docker-pull-new" }
        | "service:infra_www1" ->
            {
              n with
              job = Some "2026-11-16/090400-docker-service-new1";
              update = Some State.Running;
            }
        | "service:infra_www2" -> { n with update = Some State.Ready }
        | "notify" -> { n with job = Some "2026-11-16/090100-slack-post-started" }
        | _ -> n)
  in
  let t = Tracker.create (target (Some "ea9bd21")) in
  let t, _ = Tracker.step t ~stages ~facts:(synthetic_facts ()) ~includes:no_includes in
  Alcotest.(check (list (pair string string)))
    "statuses"
    [
      ("build:linux-x86_64", "done");
      ("push:ocurrent/v3.ocaml.org-server:live", "done");
      ("pull:infra_www2", "done");
      ("pull:infra_www1", "done");
      ("service:infra_www2", "queued");
      ("service:infra_www1", "running");
      ("notify", "pending");
    ]
    (statuses t);
  Alcotest.check verdict "verdict" Tracker.In_progress (Tracker.verdict t)

let test_failure () =
  let stages =
    rewrite (main_stages ()) (fun key (n : Svg_graph.node) ->
        match key with
        | "build:linux-x86_64" ->
            { n with job = Some "2026-11-16/090000-ocluster-build-new" }
        | "push:ocurrent/v3.ocaml.org-server:live" ->
            {
              n with
              job = Some "2026-11-16/090200-docker-push-manifest-new";
              state = State.Failed;
              tooltip = Some "push failed";
            }
        | _ -> n)
  in
  let t = Tracker.create (target (Some "ea9bd21")) in
  let t, events =
    Tracker.step t ~stages
      ~facts:(synthetic_facts ~push_outcome:(`Failed "denied") ())
      ~includes:no_includes
  in
  Alcotest.check verdict "verdict"
    (Tracker.Failed_at "push:ocurrent/v3.ocaml.org-server:live") (Tracker.verdict t);
  Alcotest.(check bool)
    "finished event" true
    (List.exists (function Tracker.Finished (Failed_at _) -> true | _ -> false) events)

let test_blocked () =
  (* A node that never ran (no job) but is grey is blocked, not pending. *)
  let stages =
    rewrite (main_stages ()) (fun key (n : Svg_graph.node) ->
        match key with
        | "service:infra_www1" -> { n with job = None; state = State.Blocked }
        | _ -> n)
  in
  let t = Tracker.create (target None) in
  let t, _ = Tracker.step t ~stages ~facts:fixture_facts ~includes:no_includes in
  Alcotest.(check (option string))
    "blocked" (Some "blocked")
    (List.assoc_opt "service:infra_www1" (statuses t))

let test_sha_matches () =
  Alcotest.(check bool)
    "prefix" true
    (Tracker.sha_matches "ea9bd21" "ea9bd215503464270bcc");
  Alcotest.(check bool)
    "too short" false
    (Tracker.sha_matches "ea9b" "ea9bd215503464270bcc");
  Alcotest.(check bool)
    "different" false
    (Tracker.sha_matches "ea9bd22" "ea9bd215503464270bcc")

(* --- Target --- *)

let test_target () =
  let ok s expected =
    match Target.parse s with
    | Ok t -> Alcotest.(check string) s expected (Target.to_string t)
    | Error e -> Alcotest.failf "%s: %s" s e
  in
  let bad s =
    match Target.parse s with
    | Ok t -> Alcotest.failf "%s parsed as %s" s (Target.to_string t)
    | Error _ -> ()
  in
  ok "ocaml/ocaml.org#1234" "ocaml/ocaml.org#1234";
  ok "ocaml/ocaml.org@EA9BD2155" "ocaml/ocaml.org@ea9bd2155";
  ok "ocaml/ocaml.org:staging" "ocaml/ocaml.org:staging";
  ok "ocaml/ocaml.org" "ocaml/ocaml.org:main";
  bad "ocaml.org#12";
  bad "ocaml/ocaml.org#x";
  bad "ocaml/ocaml.org@xyz";
  bad "ocaml/ocaml.org@abc12";
  bad "ocaml/ocaml.org:"

let () =
  Alcotest.run "oeil"
    [
      ( "svg",
        [
          Alcotest.test_case "top-level view" `Quick test_top;
          Alcotest.test_case "update gradient" `Quick test_gradient;
        ] );
      ( "chain",
        [
          Alcotest.test_case "ocaml.org main" `Quick test_chain;
          Alcotest.test_case "missing branch" `Quick test_chain_missing;
        ] );
      ( "job_log",
        [
          Alcotest.test_case "build" `Quick test_build_log;
          Alcotest.test_case "push-manifest" `Quick test_manifest_log;
          Alcotest.test_case "service & pull" `Quick test_service_log;
          Alcotest.test_case "failed" `Quick test_failed_log;
          Alcotest.test_case "stream" `Quick test_stream;
        ] );
      ("target", [ Alcotest.test_case "parse" `Quick test_target ]);
      ( "tracker",
        [
          Alcotest.test_case "already deployed" `Quick test_already_deployed;
          Alcotest.test_case "not built yet" `Quick test_not_built_yet;
          Alcotest.test_case "superseded" `Quick test_superseded;
          Alcotest.test_case "in progress" `Quick test_in_progress;
          Alcotest.test_case "failure" `Quick test_failure;
          Alcotest.test_case "blocked" `Quick test_blocked;
          Alcotest.test_case "sha prefix" `Quick test_sha_matches;
        ] );
    ]
