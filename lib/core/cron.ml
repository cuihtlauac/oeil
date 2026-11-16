(* GitHub Actions cron schedules (five fields, UTC) and their next run. *)

type field = int -> bool

type t = {
  minute : field;
  hour : field;
  dom : field;
  month : field;
  dow : field;
  text : string;
}

let field ~lo ~hi s : field option =
  let range r =
    match String.split_on_char '/' r with
    | [ base; step ] -> (
        match int_of_string_opt step with
        | Some st when st > 0 ->
            let a, b =
              match String.split_on_char '-' base with
              | [ "*" ] -> (lo, hi)
              | [ a ] -> (int_of_string a, hi)
              | [ a; b ] -> (int_of_string a, int_of_string b)
              | _ -> raise Exit
            in
            Some (fun v -> v >= a && v <= b && (v - a) mod st = 0)
        | _ -> None)
    | [ "*" ] -> Some (fun _ -> true)
    | [ base ] -> (
        match String.split_on_char '-' base with
        | [ a ] ->
            let a = int_of_string a in
            Some (fun v -> v = a)
        | [ a; b ] ->
            let a = int_of_string a and b = int_of_string b in
            Some (fun v -> v >= a && v <= b)
        | _ -> None)
    | _ -> None
  in
  try
    let parts = List.map range (String.split_on_char ',' s) in
    if List.mem None parts then None
    else
      let ps = List.filter_map Fun.id parts in
      Some (fun v -> List.exists (fun p -> p v) ps)
  with _ -> None

(* Day and month names, as cron allows ("MON", "JAN-MAR"): [names first l s]
   replaces each name of [l] in [s] by its number, counting from [first]. *)
let replace_all s ~sub ~by =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i > String.length s - n then Buffer.add_substring b s i (String.length s - i)
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let names first l s =
  List.fold_left
    (fun (s, i) n -> (replace_all s ~sub:n ~by:(string_of_int i), i + 1))
    (String.uppercase_ascii s, first)
    l
  |> fst

let parse text =
  match String.split_on_char ' ' (String.trim text) |> List.filter (( <> ) "") with
  | [ mi; h; dom; mo; dow ] -> (
      let dow = names 0 [ "SUN"; "MON"; "TUE"; "WED"; "THU"; "FRI"; "SAT" ] dow in
      let mo =
        names 1
          [
            "JAN";
            "FEB";
            "MAR";
            "APR";
            "MAY";
            "JUN";
            "JUL";
            "AUG";
            "SEP";
            "OCT";
            "NOV";
            "DEC";
          ]
          mo
      in
      (* Sunday is 0 or 7. *)
      let dow =
        match field ~lo:0 ~hi:7 dow with
        | Some f -> Some (fun d -> f d || (d = 0 && f 7))
        | None -> None
      in
      match
        ( field ~lo:0 ~hi:59 mi,
          field ~lo:0 ~hi:23 h,
          field ~lo:1 ~hi:31 dom,
          field ~lo:1 ~hi:12 mo,
          dow )
      with
      | Some minute, Some hour, Some dom, Some month, Some dow ->
          Some { minute; hour; dom; month; dow; text }
      | _ -> None)
  | _ -> None

(* The first time strictly after [now] (seconds, UTC) the schedule fires,
   within eight days. *)
let next t ~now =
  let start = ((Float.to_int now / 60) + 1) * 60 in
  let rec go i =
    if i > 8 * 1440 then None
    else
      let s = float_of_int (start + (i * 60)) in
      match Ptime.of_float_s s with
      | None -> None
      | Some p ->
          let (_, mo, d), ((h, mi, _), _) = Ptime.to_date_time p in
          let wd =
            match Ptime.weekday p with
            | `Sun -> 0
            | `Mon -> 1
            | `Tue -> 2
            | `Wed -> 3
            | `Thu -> 4
            | `Fri -> 5
            | `Sat -> 6
          in
          if t.minute mi && t.hour h && t.dom d && t.month mo && t.dow wd then Some s
          else go (i + 1)
  in
  go 0

(* "cron: '36 5 * * *'" lines of a workflow file. *)
let of_workflow source =
  String.split_on_char '\n' source
  |> List.filter_map (fun l ->
      let l = String.trim l in
      let l =
        if String.starts_with ~prefix:"- " l then String.sub l 2 (String.length l - 2)
        else l
      in
      if String.starts_with ~prefix:"cron:" l then
        let v = String.trim (String.sub l 5 (String.length l - 5)) in
        let v =
          if String.length v >= 2 && (v.[0] = '\'' || v.[0] = '"') then
            String.sub v 1 (String.length v - 2)
          else v
        in
        parse v
      else None)
