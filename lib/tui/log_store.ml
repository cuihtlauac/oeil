(* Job logs fetched for the log view, by job id. *)

let max_lines = 100_000

type log = {
  lines : string Dynarray.t;
  mutable dropped : int;  (** lines dropped from the start past [max_lines] *)
  mutable complete : bool;
  mutable error : string option;
}

type t = (string, log) Hashtbl.t

let create () : t = Hashtbl.create 8
let find (t : t) job = Hashtbl.find_opt t job

let add (t : t) job =
  let l = { lines = Dynarray.create (); dropped = 0; complete = false; error = None } in
  Hashtbl.replace t job l;
  l

let clean s = String.map (function '\t' -> ' ' | c when c < ' ' -> ' ' | c -> c) s

let push l line =
  if Dynarray.length l.lines >= max_lines then begin
    (* Drop the oldest tenth at once rather than one line at a time. *)
    let keep = Dynarray.length l.lines - (max_lines / 10) in
    let tail =
      Dynarray.init keep (fun i -> Dynarray.get l.lines (i + (max_lines / 10)))
    in
    Dynarray.clear l.lines;
    Dynarray.append l.lines tail;
    l.dropped <- l.dropped + (max_lines / 10)
  end;
  Dynarray.add_last l.lines (clean line)
