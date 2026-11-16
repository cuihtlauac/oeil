type kind =
  | Build of string
  | Push of string
  | Pull of string
  | Service of string
  | Notify

type stage = { key : string; kind : kind; node : Svg_graph.node }

let kind_name = function
  | Build _ -> "build"
  | Push _ -> "push"
  | Pull _ -> "pull"
  | Service _ -> "service"
  | Notify -> "notify"

let kind_label = function
  | Build p -> "build " ^ p
  | Push t -> "push " ^ t
  | Pull "" -> "pull"
  | Pull s -> "pull for " ^ s
  | Service s -> "service " ^ s
  | Notify -> "notify"

let describe s = kind_label s.kind
let ref_label ~repo ~branch = [ repo; "refs/heads/" ^ branch ]

let branches (g : Svg_graph.t) ~repo =
  List.filter_map
    (fun (n : Svg_graph.node) ->
      match n.label with
      | [ r; ref ] when r = repo && String.starts_with ~prefix:"refs/heads/" ref ->
          Some (String.sub ref 11 (String.length ref - 11))
      | _ -> None)
    g.nodes

let ref_node (g : Svg_graph.t) ~repo ~branch =
  let l = ref_label ~repo ~branch in
  List.find_opt (fun (n : Svg_graph.node) -> n.label = l) g.nodes

let classify (g : Svg_graph.t) (n : Svg_graph.node) =
  let rest = match n.label with _ :: r -> r | [] -> [] in
  let first = match n.label with f :: _ -> f | [] -> "" in
  let word =
    match String.index_opt first ' ' with Some i -> String.sub first 0 i | None -> first
  in
  match word with
  | "build" ->
      let platform = match rest with p :: _ -> p | [] -> "" in
      Some (Build platform)
  | "docker-push-manifest" -> Some (Push (String.concat "" rest))
  | "pull" ->
      let service =
        Svg_graph.succs g n.id
        |> List.find_map (fun (s : Svg_graph.node) ->
            match s.label with
            | ( "docker-service" | "docker-compose" | "docker-compose-v2"
              | "docker-compose-cli" )
              :: name :: _ ->
                Some name
            | _ -> None)
      in
      Some (Pull (Option.value service ~default:""))
  (* A Docker Compose deployment is the service stage of its chain. *)
  | "docker-service" | "docker-compose" | "docker-compose-v2" | "docker-compose-cli" ->
      Some (Service (match rest with s :: _ -> s | [] -> ""))
  | "post" -> Some Notify
  | _ -> None

let key_of = function
  | Build p -> "build:" ^ p
  | Push t -> "push:" ^ t
  | Pull s -> "pull:" ^ s
  | Service s -> "service:" ^ s
  | Notify -> "notify"

let rank = function
  | Build _ -> 0
  | Push _ -> 1
  | Pull _ -> 2
  | Service _ -> 3
  | Notify -> 4

let stages g ~repo ~branch =
  match ref_node g ~repo ~branch with
  | None ->
      let known = branches g ~repo in
      Error
        (Printf.sprintf "no node for %s refs/heads/%s in the pipeline (branches seen: %s)"
           repo branch
           (match known with [] -> "none" | l -> String.concat ", " l))
  | Some r ->
      let seen = Hashtbl.create 8 in
      Ok
        (Svg_graph.descendants g r.id
        |> List.filter_map (fun n ->
            match classify g n with
            | None -> None
            | Some kind ->
                let key = key_of kind in
                (* Disambiguate repeated keys (e.g. two unnamed pulls). *)
                let key =
                  match Hashtbl.find_opt seen key with
                  | None ->
                      Hashtbl.replace seen key 1;
                      key
                  | Some i ->
                      Hashtbl.replace seen key (i + 1);
                      Printf.sprintf "%s#%d" key (i + 1)
                in
                Some { key; kind; node = n })
        |> List.stable_sort (fun a b -> compare (rank a.kind) (rank b.kind)))
