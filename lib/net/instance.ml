open Oeil

type t = { web : Web.t; base : Uri.t }

let default_base = "https://deploy.ci.ocaml.org"
let make web base = { web; base = Uri.of_string base }
let base t = t.base

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
