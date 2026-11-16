(* Cached access to jobs. A finished job's log never changes, so its facts
   are fetched once. *)
open Oeil

type t = {
  inst : Instance.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  facts : (string, Tracker.Facts.t) Hashtbl.t;
  heads : (string, string list) Hashtbl.t;
  histories : (string, string list) Hashtbl.t;
  queries : (string, float * Instance.query_row list) Hashtbl.t;
  commits : (string, string * string) Hashtbl.t;
      (** built image digest -> commit, build job *)
}

let create ~clock inst =
  {
    inst;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    facts = Hashtbl.create 64;
    heads = Hashtbl.create 64;
    histories = Hashtbl.create 16;
    queries = Hashtbl.create 8;
    commits = Hashtbl.create 16;
  }

let instance t = t.inst

(* "2026-11-16/094513-docker-service-037a51" -> "2026-11-16/094513" *)
let time_key job = if String.length job >= 17 then String.sub job 0 17 else job

(* "2026-11-16 09:43:58" -> "2026-11-16/094358" *)
let key_of_finished f =
  if String.length f >= 19 then
    String.sub f 0 10 ^ "/" ^ String.sub f 11 2 ^ String.sub f 14 2 ^ String.sub f 17 2
  else f

(* Facts of a finished job. A job still running would keep the request open:
   give up after [timeout] seconds. *)
let facts ?(timeout = 30.) t job =
  match Hashtbl.find_opt t.facts job with
  | Some f -> Ok f
  | None -> (
      match
        Eio.Time.with_timeout t.clock timeout (fun () -> Ok (Instance.job_log t.inst job))
      with
      | Error `Timeout -> Error (job ^ ": still running")
      | Ok (Error _ as e) -> e
      | Ok (Ok log) ->
          let f = Tracker.Facts.of_log log in
          if f.outcome <> `Unknown then Hashtbl.replace t.facts job f;
          Ok f)

let head t job =
  match Hashtbl.find_opt t.heads job with
  | Some h -> Ok h
  | None ->
      Result.map
        (fun h ->
          Hashtbl.replace t.heads job h;
          h)
        (Instance.job_head t.inst ~clock:t.clock job)

let history t job =
  match Hashtbl.find_opt t.histories job with
  | Some h -> Ok h
  | None ->
      Result.map
        (fun h ->
          Hashtbl.replace t.histories job h;
          h)
        (Instance.job_history t.inst job)

(* A node's runs, newest first: its current job and those its page lists. *)
let runs t job =
  let older = match history t job with Ok h -> h | Error _ -> [] in
  List.sort_uniq (fun a b -> compare b a) (job :: older)

let today () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let query t ~op ~date =
  let key = op ^ "@" ^ date in
  let fresh (at, _) = date <> today () || Unix.gettimeofday () -. at < 60. in
  match Hashtbl.find_opt t.queries key with
  | Some ((_, rows) as c) when fresh c -> Ok rows
  | _ ->
      Result.map
        (fun rows ->
          Hashtbl.replace t.queries key (Unix.gettimeofday (), rows);
          rows)
        (Instance.query_jobs t.inst ~op ~ok:true ~date ())

(* "2026-11-16/094407" (UTC) -> seconds since the epoch. *)
let seconds_of_key k =
  try
    Scanf.sscanf k "%4d-%2d-%2d/%2d%2d%2d" (fun y mo d h mi s ->
        Option.map Ptime.to_float_s (Ptime.of_date_time ((y, mo, d), ((h, mi, s), 0))))
  with _ -> None

let previous_day date =
  try
    Scanf.sscanf date "%4d-%2d-%2d" (fun y m d ->
        match Ptime.of_date (y, m, d) with
        | None -> None
        | Some p -> (
            match Ptime.sub_span p (Ptime.Span.of_int_s 86400) with
            | None -> None
            | Some p ->
                let y, m, d = Ptime.to_date p in
                Some (Printf.sprintf "%04d-%02d-%02d" y m d)))
  with _ -> None

(* Fetch several jobs' heads or facts at once, to fill the cache. *)
let prefetch f jobs = Eio.Fiber.List.iter ~max_fibers:8 (fun j -> ignore (f j)) jobs
let prefetch_heads t jobs = prefetch (head t) jobs
let prefetch_facts t jobs = prefetch (facts t) jobs

(* The commit a build of [repo]'s [branch] built [digest] from, and that
   build's job, searching the successful builds that finished before [before]
   (a time key). *)
let commit_of_digest t ~repo ~branch ~hint ~before digests =
  match List.find_map (Hashtbl.find_opt t.commits) digests with
  | Some c -> Some c
  | None -> (
      let remember job (f : Tracker.Facts.t) =
        match (f.commit, f.output) with
        | Some c, Some d when List.mem d digests ->
            Hashtbl.replace t.commits d (c, job);
            Some (c, job)
        | _ -> None
      in
      let from_hint =
        Option.bind hint (fun job ->
            match facts ~timeout:5. t job with Ok f -> remember job f | Error _ -> None)
      in
      match from_hint with
      | Some c -> Some c
      | None ->
          let date = String.sub before 0 10 in
          let dates = date :: Option.to_list (previous_day date) in
          let wanted = Printf.sprintf "github.com/%s.git#refs/heads/%s " repo branch in
          let candidates =
            List.concat_map
              (fun date ->
                match query t ~op:"ocluster-build" ~date with Ok r -> r | Error _ -> [])
              dates
            (* The push-manifest job starts when the build ends, to the
               second: allow a little slack. *)
            |> List.filter (fun (r : Instance.query_row) ->
                match
                  (seconds_of_key (key_of_finished r.finished), seconds_of_key before)
                with
                | Some f, Some b -> f <= b +. 120.
                | _ -> key_of_finished r.finished <= before)
            |> List.sort (fun (a : Instance.query_row) b -> compare b.finished a.finished)
            |> List.filteri (fun i _ -> i < 30)
          in
          prefetch_heads t (List.map (fun (r : Instance.query_row) -> r.id) candidates);
          let ours =
            List.filter_map
              (fun (r : Instance.query_row) ->
                match head t r.id with
                | Ok h when List.exists (fun l -> Job_log.contains l ~sub:wanted) h ->
                    Some r.id
                | _ -> None)
              candidates
          in
          prefetch_facts t ours;
          List.find_map
            (fun id -> match facts t id with Ok f -> remember id f | Error _ -> None)
            ours)

(* The newest successful job of [op] in the last [days] days whose first log
   lines satisfy [matches]: how to find a node's runs when the node shows no
   job (its input is being rebuilt). *)
let latest t ~op ~matches ?(days = 3) () =
  let rec dates d n =
    if n = 0 then []
    else d :: (match previous_day d with Some p -> dates p (n - 1) | None -> [])
  in
  List.find_map
    (fun date ->
      match query t ~op ~date with
      | Error _ -> None
      | Ok rows ->
          prefetch_heads t (List.map (fun (r : Instance.query_row) -> r.id) rows);
          List.find_map
            (fun (r : Instance.query_row) ->
              match head t r.id with
              | Ok h when matches (String.concat "\n" h) -> Some r.id
              | _ -> None)
            rows)
    (dates (today ()) days)

(* {1 On-disk cache}

   What never changes once a job finished: its facts, its first lines, and
   the commit an image digest was built from. Kept per instance under
   $XDG_CACHE_HOME/oeil (or ~/.cache/oeil). *)

let magic = "oeil-jobs-cache-v2"

type disk = {
  d_facts : (string * Tracker.Facts.t) list;
  d_heads : (string * string list) list;
  d_commits : (string * (string * string)) list;
}

let cache_file t =
  let base =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d when d <> "" -> d
    | _ -> Filename.concat (Option.value (Sys.getenv_opt "HOME") ~default:".") ".cache"
  in
  let host = Option.value (Uri.host (Instance.base t.inst)) ~default:"instance" in
  Filename.concat (Filename.concat base "oeil") ("jobs-" ^ host ^ ".bin")

let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    try Sys.mkdir d 0o755 with Sys_error _ -> ()
  end

let load t =
  let file = cache_file t in
  try
    In_channel.with_open_bin file (fun ic ->
        let m = really_input_string ic (String.length magic) in
        if m = magic then begin
          let (d : disk) = Marshal.from_channel ic in
          List.iter (fun (k, v) -> Hashtbl.replace t.facts k v) d.d_facts;
          List.iter (fun (k, v) -> Hashtbl.replace t.heads k v) d.d_heads;
          List.iter (fun (k, v) -> Hashtbl.replace t.commits k v) d.d_commits
        end)
  with _ -> ()

let save t =
  let file = cache_file t in
  try
    mkdir_p (Filename.dirname file);
    let to_list h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] in
    let d =
      {
        d_facts = to_list t.facts;
        d_heads = to_list t.heads;
        d_commits = to_list t.commits;
      }
    in
    let tmp = file ^ ".tmp" in
    Out_channel.with_open_bin tmp (fun oc ->
        output_string oc magic;
        Marshal.to_channel oc d []);
    Sys.rename tmp file
  with _ -> ()
