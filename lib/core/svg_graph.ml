type node = {
  id : string;
  label : string list;
  state : State.t;
  update : State.t option;
  job : string option;
  expand : string option;
  tooltip : string option;
}

type t = { nodes : node list; edges : (string * string) list }

(* A minimal DOM. Attribute and element names keep only their local part
   ("xlink:href" becomes "href"). *)
type dom = E of string * (string * string) list * dom list | D of string

let to_dom s =
  let i = Xmlm.make_input ~strip:false ~entity:(fun _ -> None) (`String (0, s)) in
  let el ((_, name), attrs) children =
    E (name, List.map (fun ((_, k), v) -> (k, v)) attrs, children)
  in
  let data d = D d in
  match Xmlm.input_doc_tree ~el ~data i with
  | _dtd, root -> Ok root
  | exception Xmlm.Error ((l, c), e) ->
      Error (Printf.sprintf "SVG %d:%d: %s" l c (Xmlm.error_message e))

let attr k attrs = List.assoc_opt k attrs
let rec text = function D d -> d | E (_, _, cs) -> String.concat "" (List.map text cs)

let rec iter f d =
  f d;
  match d with E (_, _, cs) -> List.iter (iter f) cs | D _ -> ()

let child name = function
  | E (_, _, cs) ->
      List.find_map (function E (n, _, _) as e when n = name -> Some e | _ -> None) cs
  | D _ -> None

let find_all p d =
  let acc = ref [] in
  iter (fun d -> if p d then acc := d :: !acc) d;
  List.rev !acc

let has_class c = function E ("g", a, _) -> attr "class" a = Some c | _ -> false

(* Linear gradients: id -> stop colours, in order. Graphviz writes a stop
   either as [stop-color="#…"] or as [style="stop-color:#…;…"]. *)
let gradients root =
  let stop_colour a =
    match attr "stop-color" a with
    | Some c -> Some c
    | None -> (
        match attr "style" a with
        | None -> None
        | Some s ->
            String.split_on_char ';' s
            |> List.find_map (fun kv ->
                match String.index_opt kv ':' with
                | Some i when String.trim (String.sub kv 0 i) = "stop-color" ->
                    Some (String.trim (String.sub kv (i + 1) (String.length kv - i - 1)))
                | _ -> None))
  in
  find_all
    (function E (("linearGradient" | "radialGradient"), _, _) -> true | _ -> false)
    root
  |> List.filter_map (function
    | E (_, a, cs) -> (
        match attr "id" a with
        | None -> None
        | Some id ->
            let stops =
              List.filter_map
                (function E ("stop", a, _) -> stop_colour a | _ -> None)
                cs
            in
            Some (id, stops))
    | D _ -> None)

let strip_marker s =
  (* OCurrent appends " ✔", " ✗" or " …" to the label of a job node. *)
  let markers = [ " \u{2714}"; " \u{2717}"; " \u{2026}" ] in
  match List.find_opt (fun m -> String.ends_with ~suffix:m s) markers with
  | Some m -> String.sub s 0 (String.length s - String.length m)
  | None -> s

let fill_states ~gradients g =
  let fills =
    find_all
      (function
        | E (("polygon" | "ellipse" | "path" | "polyline" | "rect"), a, _) -> (
            match attr "fill" a with
            | Some f ->
                String.length f > 0 && (f.[0] = '#' || String.starts_with ~prefix:"url(" f)
            | None -> false)
        | _ -> false)
      g
  in
  match fills with
  | E (_, a, _) :: _ -> (
      let f = Option.get (attr "fill" a) in
      if f.[0] = '#' then (State.of_colour f, None)
      else
        (* url(#id) *)
        let id =
          let s = String.sub f 4 (String.length f - 5) in
          if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1)
          else s
        in
        match List.assoc_opt id gradients with
        | Some [ up; v ] -> (State.of_colour v, Some (State.of_colour up))
        | Some (c :: _) -> (State.of_colour c, None)
        | _ -> (State.Unknown f, None))
  | _ -> (State.Unknown "", None)

let node_of ~gradients g =
  let id = match child "title" g with Some t -> String.trim (text t) | None -> "" in
  let label =
    find_all (function E ("text", _, _) -> true | _ -> false) g
    |> List.map text
    |> List.filter (fun s -> String.trim s <> "")
  in
  let label =
    match List.rev label with
    | last :: rest -> List.rev (strip_marker last :: rest)
    | [] -> []
  in
  let label = List.filter (fun s -> s <> "") label in
  let anchor =
    find_all (function E ("a", _, _) -> true | _ -> false) g |> function
    | E (_, a, _) :: _ -> a
    | _ -> []
  in
  let href = attr "href" anchor in
  let job =
    match href with
    | Some h when String.starts_with ~prefix:"/job/" h ->
        Some (String.sub h 5 (String.length h - 5))
    | _ -> None
  in
  let expand =
    match href with
    | Some h when String.starts_with ~prefix:"/?" h ->
        Some (String.sub h 2 (String.length h - 2))
    | _ -> None
  in
  let tooltip =
    match attr "title" anchor with Some t when String.trim t <> "" -> Some t | _ -> None
  in
  let state, update = fill_states ~gradients g in
  { id; label; state; update; job; expand; tooltip }

let split_arrow s =
  let s = String.trim s in
  let n = String.length s in
  let rec go i =
    if i + 1 >= n then None
    else if s.[i] = '-' && (s.[i + 1] = '>' || s.[i + 1] = '-') then
      Some (String.trim (String.sub s 0 i), String.trim (String.sub s (i + 2) (n - i - 2)))
    else go (i + 1)
  in
  go 0

let parse s =
  match to_dom s with
  | Error _ as e -> e
  | Ok root ->
      let gradients = gradients root in
      let nodes = find_all (has_class "node") root |> List.map (node_of ~gradients) in
      let edges =
        find_all (has_class "edge") root
        |> List.filter_map (fun g ->
            match child "title" g with Some t -> split_arrow (text t) | None -> None)
      in
      Ok { nodes; edges }

let find g id = List.find_opt (fun n -> n.id = id) g.nodes
let succs g id = List.filter_map (fun (a, b) -> if a = id then find g b else None) g.edges
let preds g id = List.filter_map (fun (a, b) -> if b = id then find g a else None) g.edges

let descendants g id =
  let seen = Hashtbl.create 16 in
  Hashtbl.replace seen id ();
  let rec bfs acc = function
    | [] -> List.rev acc
    | x :: q ->
        let next =
          succs g x
          |> List.filter (fun n ->
              if Hashtbl.mem seen n.id then false
              else (
                Hashtbl.replace seen n.id ();
                true))
        in
        bfs (List.rev_append next acc) (q @ List.map (fun n -> n.id) next)
  in
  bfs [] [ id ]

let label n = String.concat " | " n.label
let effective_state n = match n.update with Some u -> u | None -> n.state
