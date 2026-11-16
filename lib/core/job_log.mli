(** Job logs, as served by an OCurrent web UI at [/job/<id>]: an HTML page whose [<pre>]
    element holds the log. *)

val of_html : string -> string list
(** [of_html page] is the log lines of a job page, with markup removed and entities
    decoded. *)

val history : string -> string list
(** [history page] is the job ids of earlier runs of the same node, newest first, from the
    page's "Build:" list. *)

(** Incremental reading of a page that is still being sent: a running job's page stays
    open and sends log data as it arrives. *)
module Stream : sig
  type t

  val create : unit -> t

  val feed : t -> string -> string list
  (** [feed t chunk] adds [chunk] and returns the log lines it completed. *)

  val finish : t -> string list
  (** [finish t] returns the last, unterminated, line if any. *)

  val finished : t -> bool
  (** [true] once the end of the log ([</pre>]) was seen. *)
end

val strip : string -> string
(** [strip line] removes HTML tags and decodes entities. *)

(** {1 Extractors} *)

val is_hex : string -> bool

val digests : string -> string list
(** [digests line] is every [sha256:<64 hex>] digest in [line], as ["sha256:…"], in order.
*)

val header : string list -> string list
(** The ["New job: …"] entry: the first log line and its continuation lines. *)

val commit : string list -> (string * string) option
(** [commit log] is [(git_ref, sha)] for a build log that starts with
    [… #refs/heads/main (<sha>)], e.g. [("refs/heads/main", "ea9bd21…")]. *)

val build_digest : string list -> string option
(** The digest of the image a build pushed (["…: digest: sha256:… size: …"]). *)

val input_digests : string list -> string list
(** Digests named in the job's {!header}: the images a push-manifest, pull or service job
    works on. *)

val output_digest : string list -> string option
(** For a push-manifest job, the digest of the manifest it published
    (["--> \"…@sha256:…\""]). *)

val outcome : string list -> [ `Succeeded | `Failed of string | `Cancelled | `Unknown ]
(** How the job ended, from its last lines. *)

val timestamp : string -> string option
(** [timestamp line] is the ["2026-11-16 07:18.46"] prefix of a log line. *)
