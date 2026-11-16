open Oeil

type t = { web : Web.t; base : Uri.t }

let default_base = "https://deploy.ci.ocaml.org"
let make web base = { web; base = Uri.of_string base }
let base t = t.base

(* Another OCurrent service, through the same HTTP client. *)
let with_base t base = { t with base = Uri.of_string base }

let url t ?query path =
  let u = Uri.with_path t.base path in
  match query with None -> u | Some q -> Uri.with_query u (Uri.query_of_encoded q)

let job_url t job = url t ("/job/" ^ job)

let get_ok t uri =
  match Web.get t.web uri with
  | Ok (200, body) -> Ok body
  | Ok (s, _) -> Error (Printf.sprintf "%s: HTTP %d" (Uri.to_string uri) s)
  | Error _ as e -> e

let pipeline_svg t ?query () = get_ok t (url t ?query "/pipeline.svg")
let pipeline t ?query () = Result.bind (pipeline_svg t ?query ()) Svg_graph.parse
let job_page t job = get_ok t (job_url t job)
let job_log t job = Result.map Job_log.of_html (job_page t job)

let follow_job ?(stop = fun () -> false) t job ~on_line =
  let st = Job_log.Stream.create () in
  let r =
    Web.stream t.web (job_url t job) ~on_chunk:(fun chunk ->
        List.iter on_line (Job_log.Stream.feed st chunk);
        if Job_log.Stream.finished st || stop () then `Stop else `Continue)
  in
  if not (stop ()) then List.iter on_line (Job_log.Stream.finish st);
  r

(* The first lines of a job's log. A running job's page only ends with the
   job, so stop after [lines] lines or [timeout] seconds. *)
let job_head t ~clock ?(lines = 6) ?(timeout = 5.0) job =
  let acc = ref [] in
  let n = ref 0 in
  let r =
    try
      Eio.Time.with_timeout_exn clock timeout (fun () ->
          follow_job t job
            ~stop:(fun () -> !n >= lines)
            ~on_line:(fun l ->
              acc := l :: !acc;
              incr n))
    with Eio.Time.Timeout -> Ok ()
  in
  Result.map (fun () -> List.rev !acc) r

(* Earlier runs of a job's node, from the "Build:" list at the top of its
   page. The page of a running job only ends with the job, so stop reading
   once the log starts. *)
let job_history t job =
  let buf = Buffer.create 4096 in
  let r =
    Web.stream t.web (job_url t job) ~on_chunk:(fun chunk ->
        Buffer.add_string buf chunk;
        if Job_log.contains (Buffer.contents buf) ~sub:"<pre>" then `Stop else `Continue)
  in
  Result.map (fun () -> Job_log.history (Buffer.contents buf)) r

type query_row = { id : string; finished : string  (** "2026-11-15 19:56:19" *) }

(* Jobs listed by the query page, e.g. [~op:"ocluster-build" ~ok:true
   ~date:"2026-11-15"], newest first. *)
let query_jobs t ~op ?ok ?date () =
  let q =
    [ ("op", op) ]
    @ (match ok with Some b -> [ ("ok", string_of_bool b) ] | None -> [])
    @ match date with Some d -> [ ("date", d) ] | None -> []
  in
  let uri = Uri.with_query' (url t "/query") q in
  Result.map
    (fun page ->
      (* Recent rows start with an extra (rebuild checkbox) cell: find the
         job id's cell; "Finished" is four cells after it. *)
      let is_id c = String.length c > 11 && c.[4] = '-' && c.[10] = '/' in
      Job_log.table_rows page
      |> List.filter_map (fun cells ->
          let rec from = function
            | id :: _ :: _ :: _ :: finished :: _ when is_id id -> Some { id; finished }
            | _ :: rest -> from rest
            | [] -> None
          in
          from cells))
    (get_ok t uri)
