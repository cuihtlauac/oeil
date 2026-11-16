type t =
  | Up
  | Down
  | Left
  | Right
  | Page_up
  | Page_down
  | Home
  | End
  | Enter
  | Escape
  | Backspace
  | Char of char
  | Unknown

(* CSI parameters then a final byte: "5~", "A", "1;5A"… *)
let csi input =
  let open Eio.Buf_read in
  let b = Buffer.create 4 in
  let rec go () =
    let c = any_char input in
    if (c >= '0' && c <= '9') || c = ';' then (
      Buffer.add_char b c;
      go ())
    else c
  in
  let final = go () in
  let param = Buffer.contents b in
  let n = match String.split_on_char ';' param with p :: _ -> p | [] -> "" in
  match (final, n) with
  | 'A', _ -> Up
  | 'B', _ -> Down
  | 'C', _ -> Right
  | 'D', _ -> Left
  | 'H', _ -> Home
  | 'F', _ -> End
  | '~', ("5" | "") -> Page_up
  | '~', "6" -> Page_down
  | '~', ("1" | "7") -> Home
  | '~', ("4" | "8") -> End
  | _ -> Unknown

let read input =
  let open Eio.Buf_read in
  match any_char input with
  | '\027' when buffered_bytes input = 0 -> Escape
  | '\027' -> (
      match peek_char input with
      | Some ('[' | 'O') ->
          skip 1 input;
          csi input
      | _ -> Escape)
  | '\r' | '\n' -> Enter
  | '\127' | '\b' -> Backspace
  | c -> Char c
