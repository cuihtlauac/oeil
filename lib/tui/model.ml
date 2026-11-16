(* The state of the interactive view, and what keys do to it. Pure. *)
open Oeil

type mode =
  | Summary
  | Log of { key : string; offset : int }
      (** [offset] is how many lines above the end the view is scrolled; [0] follows the
          end of the log. *)

type phase =
  | Resolving
  | Waiting_merge of string
  | Watching
  | Finished of [ `Deployed | `Failed of string | `Timeout ]
  | Error of string

type t = {
  title : string;
  phase : phase;
  tracker : Tracker.t option;
  selected : int;
  mode : mode;
  started : float;
  last_poll : float option;
  last_error : string option;
  quit : bool;
}

let v ~title ~now =
  {
    title;
    phase = Resolving;
    tracker = None;
    selected = 0;
    mode = Summary;
    started = now;
    last_poll = None;
    last_error = None;
    quit = false;
  }

let stages t = match t.tracker with Some tr -> Tracker.stages tr | None -> []
let selected_stage t = List.nth_opt (stages t) t.selected
let stage t key = List.find_opt (fun (s : Tracker.stage) -> s.key = key) (stages t)

(* Clamp the selection when the stages change. *)
let with_tracker t tracker =
  let n = List.length (Tracker.stages tracker) in
  { t with tracker = Some tracker; selected = max 0 (min t.selected (n - 1)) }

type action = Nothing | Open_log of string  (** job *) | Quit

let key t ~page (k : Keys.t) =
  let n = List.length (stages t) in
  match t.mode with
  | Summary -> (
      let select i = ({ t with selected = max 0 (min (n - 1) i) }, Nothing) in
      match k with
      | Up | Char 'k' -> select (t.selected - 1)
      | Down | Char 'j' -> select (t.selected + 1)
      | Home | Char 'g' -> select 0
      | End | Char 'G' -> select (n - 1)
      | Enter | Right | Char 'l' -> (
          match selected_stage t with
          | Some ({ job = Some job; _ } as s) ->
              ({ t with mode = Log { key = s.key; offset = 0 } }, Open_log job)
          | Some _ | None -> (t, Nothing))
      | Char ('q' | 'Q') | Escape -> ({ t with quit = true }, Quit)
      | _ -> (t, Nothing))
  | Log { key; offset } -> (
      let scroll o = ({ t with mode = Log { key; offset = max 0 o } }, Nothing) in
      match k with
      | Up | Char 'k' -> scroll (offset + 1)
      | Down | Char 'j' -> scroll (offset - 1)
      | Page_up | Char 'b' -> scroll (offset + page)
      | Page_down | Char ' ' -> scroll (offset - page)
      | Home | Char 'g' -> scroll max_int
      | End | Char 'G' | Char 'f' -> scroll 0
      | Escape | Left | Backspace | Char ('q' | 'Q' | 'h') ->
          ({ t with mode = Summary }, Nothing)
      | _ -> (t, Nothing))
