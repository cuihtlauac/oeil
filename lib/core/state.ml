type t =
  | Ok
  | Running
  | Ready
  | Waiting_confirmation
  | Failed
  | Blocked
  | Unknown of string

(* Colours from ocurrent's lib_term/analysis.ml ([pp_dot] and
   [colour_of_activity]). *)
let of_colour c =
  match String.lowercase_ascii (String.trim c) with
  | "#90ee90" -> Ok
  | "#ffa500" -> Running
  | "#ffff00" -> Ready
  | "#4975ff" -> Waiting_confirmation
  | "#ff4500" -> Failed
  | "#d3d3d3" -> Blocked
  | s -> Unknown s

let to_string = function
  | Ok -> "ok"
  | Running -> "running"
  | Ready -> "ready"
  | Waiting_confirmation -> "waiting-for-confirmation"
  | Failed -> "failed"
  | Blocked -> "blocked"
  | Unknown s -> "unknown(" ^ s ^ ")"

let is_active = function
  | Running | Ready | Waiting_confirmation -> true
  | Ok | Failed | Blocked | Unknown _ -> false

let equal = ( = )
let pp ppf t = Format.pp_print_string ppf (to_string t)
