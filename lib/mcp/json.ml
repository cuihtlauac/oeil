(* JSON views of oeil's data, shared by the MCP tools. *)
open Oeil

let opt f = function Some x -> f x | None -> `Null
let str s = `String s

let status (s : Tracker.status) =
  match s with
  | Failed m -> `Assoc [ ("status", `String "failed"); ("error", `String m) ]
  | s -> `Assoc [ ("status", `String (Tracker.status_to_string s)) ]

let stage ~job_url (st : Tracker.stage) =
  let fields = match status st.status with `Assoc l -> l | _ -> [] in
  `Assoc
    ([ ("key", `String st.key); ("stage", `String (Chain.kind_label st.kind)) ]
    @ fields
    @ [
        ("job", opt str st.job);
        ("url", opt (fun j -> `String (job_url j)) st.job);
        ("started", opt str st.since);
      ])

let verdict = function
  | Tracker.In_progress -> `Assoc [ ("verdict", `String "in_progress") ]
  | Deployed -> `Assoc [ ("verdict", `String "deployed") ]
  | Failed_at k -> `Assoc [ ("verdict", `String "failed"); ("failed_stage", `String k) ]

let event (e : Tracker.event) =
  match e with
  | Building c -> `Assoc [ ("event", `String "building"); ("commit", `String c) ]
  | Superseded c -> `Assoc [ ("event", `String "superseded"); ("commit", `String c) ]
  | Stage { key; before; after; job } ->
      `Assoc
        [
          ("event", `String "stage");
          ("key", `String key);
          ("from", `String (Tracker.status_to_string before));
          ("to", `String (Tracker.status_to_string after));
          ("job", opt str job);
        ]
  | Finished v -> (
      match verdict v with
      | `Assoc l -> `Assoc (("event", `String "finished") :: l)
      | j -> j)

let tracker ~job_url ~title t =
  let target = Tracker.target t in
  let built, published = Tracker.digests t in
  let v = match verdict (Tracker.verdict t) with `Assoc l -> l | _ -> [] in
  `Assoc
    ([
       ("title", `String title);
       ( "target",
         `Assoc
           [
             ("repo", `String target.repo);
             ("branch", `String target.branch);
             ("sha", opt str target.sha);
           ] );
       ("commit", opt str (Tracker.commit t));
     ]
    @ v
    @ [
        ("stages", `List (List.map (stage ~job_url) (Tracker.stages t)));
        ( "digests",
          `Assoc
            [ ("built", `List (List.map str built)); ("published", opt str published) ] );
      ])

let node (n : Svg_graph.node) =
  `Assoc
    [
      ("label", `String (Svg_graph.label n));
      ("state", `String (State.to_string (Svg_graph.effective_state n)));
      ("job", opt str n.job);
      ("expand", opt str n.expand);
      ("error", opt str n.tooltip);
    ]
