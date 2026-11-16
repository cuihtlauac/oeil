(** Following one commit through a deployment chain.

    The tracker is pure: each poll, the caller gives it the chain's stages
    ({!Chain.stages}) and the facts of the jobs those stages point to, and it returns the
    new status of every stage plus what changed. *)

(** What a job's log says, as far as tracking needs. *)
module Facts : sig
  type t = {
    commit : string option;  (** the commit a build job built *)
    output : string option;  (** digest a build or push-manifest produced *)
    inputs : string list;  (** digests a push-manifest/pull/service job used *)
    outcome : [ `Succeeded | `Failed of string | `Cancelled | `Unknown ];
    started : string option;  (** ["2026-11-16 07:18.46"] *)
    tail : string list;  (** last lines of the log, for failures *)
  }

  val of_log : string list -> t
end

type target = {
  repo : string;  (** e.g. ["ocaml/ocaml.org"] *)
  branch : string;  (** e.g. ["main"] *)
  sha : string option;
      (** Commit to follow (full or abbreviated). [None] follows whatever the branch's
          latest build is. *)
}

type status =
  | Pending  (** Not yet run for our commit. *)
  | Queued  (** Ready to run for our commit, waiting for a resource. *)
  | Needs_confirmation  (** Waiting for an admin to confirm. *)
  | Running
  | Done
  | Unchanged
      (** Our commit's image is already the one here, from an earlier job: nothing to
          redeploy. *)
  | Failed of string
  | Blocked  (** An upstream stage failed. *)

type stage = {
  key : string;
  kind : Chain.kind;
  status : status;
  job : string option;  (** The job that is ours, if any. *)
  since : string option;  (** When that job started. *)
}

type verdict = In_progress | Deployed | Failed_at of string  (** stage key *)
type t

type event =
  | Building of string  (** the commit the build is on: ours, or newer *)
  | Superseded of string  (** a newer commit that includes ours is deployed instead *)
  | Stage of { key : string; before : status; after : status; job : string option }
  | Finished of verdict

val create : target -> t
val target : t -> target

val step :
  t ->
  stages:Chain.stage list ->
  facts:(string -> Facts.t option) ->
  includes:(newer:string -> bool option) ->
  t * event list
(** [step t ~stages ~facts ~includes] updates [t] from a new poll.
    - [facts job] is the parsed log of [job], if it was fetched.
    - [includes ~newer] says whether commit [newer] contains the target commit ([None]:
      unknown yet). *)

val stages : t -> stage list
val verdict : t -> verdict

val commit : t -> string option
(** The commit being deployed: the target, or a newer one that superseded it. *)

val digests : t -> string list * string option
(** Image digests followed so far: the built ones, and the published one. *)

val jobs_to_fetch : Chain.stage list -> t -> string list
(** Jobs whose logs the next {!step} needs: those of every stage node. *)

val status_to_string : status -> string
val is_final : status -> bool

val sha_matches : string -> string -> bool
(** [sha_matches a b] if one is a prefix of the other (at least 7 hex chars). *)
