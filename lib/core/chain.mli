(** The deployment chain of one branch, as drawn by ocurrent-deployer:
    [<repo> | refs/heads/<branch>] → build(s) → push-manifest → pull → docker-service → …
    → post. *)

type kind =
  | Build of string  (** platform, e.g. ["linux-x86_64"] *)
  | Push of string  (** target tag, e.g. ["ocurrent/v3.ocaml.org-server:live"] *)
  | Pull of string  (** the service it feeds, if known *)
  | Service of string  (** service name, e.g. ["infra_www1"] *)
  | Notify  (** Slack post *)

type stage = {
  key : string;  (** Stable across polls, e.g. ["service:infra_www1"]. *)
  kind : kind;
  node : Svg_graph.node;
}

val kind_name : kind -> string
(** ["build"], ["push"], ["pull"], ["service"] or ["notify"]. *)

val describe : stage -> string
(** e.g. ["build linux-x86_64"], ["service infra_www1"]. *)

val branches : Svg_graph.t -> repo:string -> string list
(** Branches of [repo] that have a ref node in the (expanded) graph. *)

val ref_node : Svg_graph.t -> repo:string -> branch:string -> Svg_graph.node option

val stages : Svg_graph.t -> repo:string -> branch:string -> (stage list, string) result
(** The stages downstream of the branch's ref node, in pipeline order. Nodes that are not
    stages (e.g. ["state"]) are left out. *)
