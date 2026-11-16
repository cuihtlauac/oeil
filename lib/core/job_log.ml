let decode_entity e =
  match e with
  | "quot" -> Some "\""
  | "amp" -> Some "&"
  | "lt" -> Some "<"
  | "gt" -> Some ">"
  | "apos" -> Some "'"
  | "nbsp" -> Some " "
  | _ when String.length e > 1 && e.[0] = '#' -> (
      let n =
        if e.[1] = 'x' || e.[1] = 'X' then
          int_of_string_opt ("0x" ^ String.sub e 2 (String.length e - 2))
        else int_of_string_opt (String.sub e 1 (String.length e - 1))
      in
      match n with
      | Some n when Uchar.is_valid n ->
          let b = Buffer.create 4 in
          Buffer.add_utf_8_uchar b (Uchar.of_int n);
          Some (Buffer.contents b)
      | _ -> None)
  | _ -> None

let strip s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else
      match s.[i] with
      | '<' -> (
          match String.index_from_opt s i '>' with
          | Some j -> go (j + 1)
          | None -> Buffer.add_substring b s i (n - i))
      | '&' -> (
          match String.index_from_opt s i ';' with
          | Some j when j - i <= 10 -> (
              match decode_entity (String.sub s (i + 1) (j - i - 1)) with
              | Some d ->
                  Buffer.add_string b d;
                  go (j + 1)
              | None ->
                  Buffer.add_char b '&';
                  go (i + 1))
          | _ ->
              Buffer.add_char b '&';
              go (i + 1))
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  Buffer.contents b

let find_sub ?(from = 0) s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1)
  in
  go from

let rfind_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i < 0 then None else if String.sub s i m = sub then Some i else go (i - 1)
  in
  go (n - m)

let split_lines s =
  let l = String.split_on_char '\n' s in
  (* Drop a trailing empty line from a final '\n'. *)
  match List.rev l with
  | "" :: rest -> List.rev rest
  | _ -> l

let of_html page =
  match find_sub page "<pre>" with
  | None -> []
  | Some i ->
      let start = i + 5 in
      let stop =
        match rfind_sub page "</pre>" with
        | Some j when j >= start -> j
        | _ -> String.length page
      in
      String.sub page start (stop - start) |> split_lines |> List.map strip

let history page =
  match (find_sub page "class=\"build-history\"", find_sub page "</ol>") with
  | Some i, Some j when j > i ->
      let seg = String.sub page i (j - i) in
      let rec collect from acc =
        match find_sub ~from seg "href=\"/job/" with
        | None -> List.rev acc
        | Some k -> (
            let s = k + String.length "href=\"/job/" in
            match String.index_from_opt seg s '"' with
            | Some e -> collect e (String.sub seg s (e - s) :: acc)
            | None -> List.rev acc)
      in
      collect 0 []
  | _ -> []

module Stream = struct
  type t = { buf : Buffer.t; mutable in_pre : bool; mutable finished : bool }

  let create () = { buf = Buffer.create 4096; in_pre = false; finished = false }
  let finished t = t.finished

  let feed t chunk =
    if t.finished then []
    else begin
      Buffer.add_string t.buf chunk;
      let s = Buffer.contents t.buf in
      let s =
        if t.in_pre then s
        else
          match find_sub s "<pre>" with
          | Some i ->
              t.in_pre <- true;
              String.sub s (i + 5) (String.length s - i - 5)
          | None ->
              (* Keep only a tail that could hold a split "<pre>". *)
              let n = String.length s in
              Buffer.clear t.buf;
              Buffer.add_string t.buf (String.sub s (max 0 (n - 4)) (min n 4));
              ""
      in
      if not t.in_pre then []
      else
        let s, ended =
          match find_sub s "</pre>" with
          | Some j ->
              t.finished <- true;
              (String.sub s 0 j, true)
          | None -> (s, false)
        in
        Buffer.clear t.buf;
        let lines = String.split_on_char '\n' s in
        let complete, rest =
          match List.rev lines with
          | last :: rev_complete -> (List.rev rev_complete, last)
          | [] -> ([], "")
        in
        if ended then List.map strip (complete @ if rest = "" then [] else [ rest ])
        else begin
          Buffer.add_string t.buf rest;
          List.map strip complete
        end
    end

  let finish t =
    let s = Buffer.contents t.buf in
    Buffer.clear t.buf;
    if t.in_pre && s <> "" then [ strip s ] else []
end

let is_hex s =
  String.length s > 0
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
       s

let digests line =
  let n = String.length line in
  let rec go from acc =
    match find_sub ~from line "sha256:" with
    | None -> List.rev acc
    | Some i ->
        let s = i + 7 in
        if s + 64 <= n && is_hex (String.sub line s 64) then
          go (s + 64) (String.sub line i (7 + 64) :: acc)
        else go s acc
  in
  go 0 []

let timestamp line =
  (* "2026-11-16 07:18.46: …" *)
  if
    String.length line >= 20
    && line.[4] = '-'
    && line.[7] = '-'
    && line.[10] = ' '
    && line.[13] = ':'
    && line.[16] = '.'
    && line.[19] = ':'
  then Some (String.sub line 0 19)
  else None

let header = function
  | [] -> []
  | first :: rest ->
      let rec take acc = function
        | l :: ls when timestamp l = None && String.length l > 0 && l.[0] = ' ' ->
            take (l :: acc) ls
        | _ -> List.rev acc
      in
      first :: take [] rest

let commit log =
  let lines = List.filteri (fun i _ -> i < 5) log in
  List.find_map
    (fun l ->
      match find_sub l "#refs/" with
      | None -> None
      | Some i -> (
          match find_sub ~from:i l " (" with
          | None -> None
          | Some j -> (
              let r = String.sub l (i + 1) (j - i - 1) in
              match String.index_from_opt l (j + 2) ')' with
              | Some k ->
                  let sha = String.sub l (j + 2) (k - j - 2) in
                  if String.length sha >= 7 && is_hex sha then Some (r, sha) else None
              | None -> None)))
    lines

let build_digest log =
  List.fold_left
    (fun acc l ->
      match find_sub l ": digest: sha256:" with
      | Some _ -> ( match digests l with d :: _ -> Some d | [] -> acc)
      | None -> acc)
    None log

let input_digests log =
  header log |> List.concat_map digests
  |> List.fold_left (fun acc d -> if List.mem d acc then acc else acc @ [ d ]) []

let output_digest log =
  List.fold_left
    (fun acc l ->
      match find_sub l ": --> " with
      | Some _ -> ( match digests l with d :: _ -> Some d | [] -> acc)
      | None -> acc)
    None log

let outcome log =
  let tail = List.rev log |> List.filteri (fun i _ -> i < 5) in
  let ends_with_msg l key =
    match find_sub l key with
    | Some i ->
        Some
          (String.trim
             (String.sub l
                (i + String.length key)
                (String.length l - i - String.length key)))
    | None -> None
  in
  let rec go = function
    | [] -> `Unknown
    | l :: ls -> (
        if find_sub l "Job succeeded" <> None then `Succeeded
        else
          match ends_with_msg l "Job failed:" with
          | Some m when String.starts_with ~prefix:"Cancelled" m -> `Cancelled
          | Some m -> `Failed m
          | None -> go ls)
  in
  go tail

let contains s ~sub = Option.is_some (find_sub s sub)

(* The rows of the HTML tables in [page], each a list of its cells' text. *)
let table_rows page =
  let rec rows from acc =
    match find_sub ~from page "<tr" with
    | None -> List.rev acc
    | Some i ->
        let stop =
          match find_sub ~from:(i + 3) page "</tr>" with
          | Some j -> j
          | None -> String.length page
        in
        let row = String.sub page i (stop - i) in
        let rec cells from acc =
          match find_sub ~from row "<td" with
          | None -> List.rev acc
          | Some k -> (
              match String.index_from_opt row k '>' with
              | None -> List.rev acc
              | Some open_end ->
                  let close =
                    match find_sub ~from:open_end row "</td>" with
                    | Some c -> c
                    | None -> String.length row
                  in
                  let cell =
                    String.trim
                      (strip (String.sub row (open_end + 1) (close - open_end - 1)))
                  in
                  cells close (cell :: acc))
        in
        rows stop (cells 0 [] :: acc)
  in
  rows 0 [] |> List.filter (fun r -> r <> [])
