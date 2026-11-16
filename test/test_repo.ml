open Oeil

let c ?(parents = []) sha = { Repo_state.sha; parents; message = "m " ^ sha; date = "" }

let pr n merge_commit =
  {
    Repo_state.number = n;
    title = "pr " ^ string_of_int n;
    merge_commit;
    merged_at = "";
    author = "";
  }

(* A branch: live at "a000000", then b, c (in flight), d, e (head). *)
let chain =
  [
    c "eeeeeee" ~parents:[ "ddddddd" ];
    c "ddddddd" ~parents:[ "ccccccc" ];
    c "ccccccc" ~parents:[ "bbbbbbb" ];
    c "bbbbbbb" ~parents:[ "a000000" ];
  ]

let summary entries =
  List.map
    (fun (e : Repo_state.entry) ->
      ( (match e.pr with Some p -> "#" ^ string_of_int p.number | None -> e.commit.sha),
        Repo_state.state_to_string e.state ))
    entries

let first_parent () =
  (* A merge commit brings in a side branch: only the first parents count. *)
  let commits =
    [
      c "mmmmmmm" ~parents:[ "bbbbbbb"; "sssssss" ];
      c "sssssss" ~parents:[ "a000000" ];
      c "bbbbbbb" ~parents:[ "a000000" ];
    ]
  in
  Alcotest.(check (list string))
    "chain" [ "mmmmmmm"; "bbbbbbb" ]
    (List.map
       (fun (c : Repo_state.commit) -> c.sha)
       (Repo_state.first_parent ~head:"mmmmmmm" commits))

let classify () =
  let prs =
    [
      pr 5 "eeeeeee";
      pr 4 "ddddddd";
      pr 3 "ccccccc";
      pr 2 "bbbbbbb";
      pr 1 "a000000";
      pr 0 "9999999";
    ]
  in
  let deploy =
    {
      Repo_state.at = "2026-11-16/094513-docker-service-x";
      digest = "sha256:x";
      commit = Some "a000000";
      push_job = None;
      build_job = None;
    }
  in
  let entries =
    Repo_state.classify ~prs ~after_live:chain
      ~inflight:(Some ("ccccccc", `Running "build running"))
      ~went_live:[ (deploy, [ c "a000000" ~parents:[ "9999999" ] ]) ]
  in
  Alcotest.(check (list (pair string string)))
    "states"
    [
      ("#5", "waiting");
      ("#4", "waiting");
      ("#3", "deploying: build running");
      ("#2", "deploying: build running");
      ("#1", "live");
    ]
    (summary entries);
  (* The same, but the in-flight deploy failed. *)
  let entries =
    Repo_state.classify ~prs ~after_live:chain
      ~inflight:(Some ("ccccccc", `Failed "push:x"))
      ~went_live:[]
  in
  Alcotest.(check (list (pair string string)))
    "failed"
    [
      ("#5", "waiting");
      ("#4", "waiting");
      ("#3", "failed at push:x");
      ("#2", "failed at push:x");
    ]
    (summary entries);
  (* Nothing in flight: everything after the live commit waits. *)
  let entries = Repo_state.classify ~prs ~after_live:chain ~inflight:None ~went_live:[] in
  Alcotest.(check int)
    "all waiting" 4
    (List.length (List.filter (fun (e : Repo_state.entry) -> e.state = Waiting) entries))

let by_pr () =
  (* A rebase merge: two commits, one PR; a direct push has none. *)
  let p = pr 7 "ddddddd" in
  let e ?pr sha state = { Repo_state.pr; commit = c sha; state } in
  let entries =
    [ e ~pr:p "ddddddd" Waiting; e ~pr:p "ccccccc" Waiting; e "bbbbbbb" Waiting ]
  in
  Alcotest.(check (list (pair string string)))
    "grouped"
    [ ("#7", "waiting"); ("bbbbbbb", "waiting") ]
    (summary (Repo_state.by_pr entries))

let table_rows () =
  (* The query page's rows; recent ones start with a checkbox cell. *)
  let page =
    {|<table><tr><th>Job</th></tr>
<tr><td><input type="checkbox"/></td><td><a href="/job/2026-11-16/100252-ocluster-build-8b3e75">2026-11-16/100252-ocluster-build-8b3e75</a></td><td>0</td><td>OK</td><td>-</td><td>2026-11-16 10:25:59</td><td>2min / 20min</td></tr>
<tr><td><a href="/job/2026-11-15/194335-ocluster-build-daf155">2026-11-15/194335-ocluster-build-daf155</a></td><td>0</td><td>OK</td><td>-</td><td>2026-11-15 19:56:19</td><td>0 / 12min</td></tr></table>|}
  in
  let rows = Job_log.table_rows page in
  Alcotest.(check int) "rows" 2 (List.length rows);
  Alcotest.(check (list string))
    "second row"
    [
      "2026-11-15/194335-ocluster-build-daf155";
      "0";
      "OK";
      "-";
      "2026-11-15 19:56:19";
      "0 / 12min";
    ]
    (List.nth rows 1)

let () =
  Alcotest.run "oeil-repo"
    [
      ( "repo_state",
        [
          Alcotest.test_case "first parent" `Quick first_parent;
          Alcotest.test_case "classify" `Quick classify;
          Alcotest.test_case "one entry per PR" `Quick by_pr;
        ] );
      ("query page", [ Alcotest.test_case "table rows" `Quick table_rows ]);
    ]
