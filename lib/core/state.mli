(** State of a pipeline node, as encoded by the fill colour of its box in an OCurrent
    pipeline diagram. *)

type t =
  | Ok  (** Green: has a value. *)
  | Running  (** Orange: a job is running. *)
  | Ready  (** Yellow: ready to run, waiting for a resource. *)
  | Waiting_confirmation  (** Blue: needs a human to confirm. *)
  | Failed  (** Red: failed by itself. *)
  | Blocked  (** Grey: an input failed. *)
  | Unknown of string  (** Any other colour. *)

val of_colour : string -> t
(** [of_colour "#90ee90"] is [Ok]. Case and surrounding spaces are ignored. *)

val to_string : t -> string

val is_active : t -> bool
(** [is_active s] is [true] for [Running], [Ready] and [Waiting_confirmation]. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
