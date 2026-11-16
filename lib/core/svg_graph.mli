(** The pipeline graph, read back from the SVG that OCurrent's web UI renders with
    Graphviz ([/pipeline.svg]). *)

type node = {
  id : string;  (** Graphviz node name, e.g. ["n19"]. Not stable across views. *)
  label : string list;
      (** Label lines, without the job marker (✔, ✗, …) OCurrent appends. *)
  state : State.t;  (** State of the node's current value. *)
  update : State.t option;
      (** When a job is re-running while the node keeps its previous value, the state of
          that update. OCurrent draws this as a two-colour fill. *)
  job : string option;  (** Job id, e.g. ["2026-11-16/071954-docker-service-f92f02"]. *)
  expand : string option;
      (** For a collapsed group, the query that expands it, e.g. ["repo=ocaml/ocaml.org"].
      *)
  tooltip : string option;  (** The error message of a failed node. *)
}

type t = { nodes : node list; edges : (string * string) list }

val parse : string -> (t, string) result
(** [parse svg] reads the nodes and edges of a Graphviz SVG. *)

val find : t -> string -> node option
val succs : t -> string -> node list
val preds : t -> string -> node list

val descendants : t -> string -> node list
(** [descendants g id] is every node reachable from [id], excluding [id], in breadth-first
    order. *)

val label : node -> string
(** The label lines joined with [" | "]. *)

val effective_state : node -> State.t
(** [update] if there is one, else [state]. *)
