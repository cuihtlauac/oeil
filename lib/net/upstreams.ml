(* What ocaml.org is built from, beyond its own repository: the docs CI
   (on dill), opam-health-check (check.ci.ocaml.org), and the compiler
   manuals (ocaml-web/html-compiler-manuals). *)
open Oeil

let docs_ci_base = "https://dill.caelum.ci.dev"
let health_base = "https://check.ci.ocaml.org"
let manuals_repo = "ocaml-web/html-compiler-manuals"

(* check.ci.ocaml.org turns away clients that do not look like a browser.
   ocaml.org's own scraper (tool/data-scrape) sends this User-Agent; oeil
   sends the same, there only. *)
let health_user_agent =
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) \
   Chrome/91.0.4472.114 Safari/537.36"

type deploy = {
  d_repo : string;
  d_branch : string;
  d_stages : Chain.stage list;
  d_state : Repo_monitor.deploy_state option;
  d_error : string option;
}

type docs = {
  deploy : deploy;
  pipeline : Svg_graph.t option;
  pipeline_at : float option;
  pipeline_error : string option;
}

type health_run = { run_id : string; run_time : float option; opam_commit : string }

type health = {
  latest : health_run option;  (** check.ci's latest run *)
  on_ocamlorg : string option;  (** the run ocaml.org's data/opam_health.yml has *)
  health_deploy : deploy;
  health_error : string option;
}

type manuals = {
  commit : Repo_state.commit option;  (** the manuals' latest commit *)
  included : bool option;  (** in the live ocaml.org build? *)
  live_build : float option;  (** when the live ocaml.org image was built *)
  manuals_error : string option;
}

type t = { docs : docs; health : health; manuals : manuals }

type ctx = {
  monitor : Repo_monitor.ctx;
  web : Web.t;
  mutable docs_graph : (float * Svg_graph.t) option;
}

let create ~monitor ~web = { monitor; web; docs_graph = None }

(* The deploy of [repo]'s [branch], by deploy.ci.ocaml.org. *)
let deploy ctx ~repo ~branch ~since =
  let inst = Jobs.instance (Repo_monitor.jobs ctx.monitor) in
  match Instance.pipeline inst ~query:("repo=" ^ repo) () with
  | Error e ->
      {
        d_repo = repo;
        d_branch = branch;
        d_stages = [];
        d_state = None;
        d_error = Some e;
      }
  | Ok g -> (
      match Chain.stages g ~repo ~branch with
      | Error e ->
          {
            d_repo = repo;
            d_branch = branch;
            d_stages = [];
            d_state = None;
            d_error = Some e;
          }
      | Ok stages ->
          let st = Repo_monitor.deploy_state ctx.monitor ~repo ~branch ~since stages in
          {
            d_repo = repo;
            d_branch = branch;
            d_stages = stages;
            d_state = Some st;
            d_error = None;
          })

(* dill's pipeline takes a while to draw (~15 s): look at most every five
   minutes. *)
let docs_pipeline ctx =
  let fresh =
    match ctx.docs_graph with
    | Some (at, _) -> Unix.gettimeofday () -. at < 300.
    | None -> false
  in
  if fresh then Ok ctx.docs_graph
  else
    let inst = Instance.make ctx.web docs_ci_base in
    match Instance.pipeline inst () with
    | Ok g ->
        ctx.docs_graph <- Some (Unix.gettimeofday (), g);
        Ok ctx.docs_graph
    | Error e -> Error e

let docs ctx ~since =
  let deploy = deploy ctx ~repo:"ocurrent/ocaml-docs-ci" ~branch:"dill" ~since in
  match docs_pipeline ctx with
  | Ok g ->
      {
        deploy;
        pipeline = Option.map snd g;
        pipeline_at = Option.map fst g;
        pipeline_error = None;
      }
  | Error e -> { deploy; pipeline = None; pipeline_at = None; pipeline_error = Some e }

(* The first "/log/<seconds>-<commit>/" link of a check.ci page names its
   latest run, as ocaml.org's scraper reads it. *)
let run_of_page page =
  let n = String.length page in
  let parse s =
    match String.index_from_opt page s '/' with
    | None -> None
    | Some e -> (
        let id = String.sub page s (e - s) in
        match String.split_on_char '-' id with
        | [ t; c ]
          when String.length c = 40 && Job_log.is_hex c && int_of_string_opt t <> None ->
            Some
              {
                run_id = id;
                run_time = Option.map float_of_int (int_of_string_opt t);
                opam_commit = c;
              }
        | _ -> None)
  in
  let rec scan i =
    if i + 5 > n then None
    else if String.sub page i 5 = "/log/" then
      match parse (i + 5) with Some r -> Some r | None -> scan (i + 5)
    else scan (i + 1)
  in
  scan 0

let health ctx ~gh ~since =
  let health_deploy =
    deploy ctx ~repo:"ocurrent/opam-health-check" ~branch:"live" ~since
  in
  let latest, err1 =
    match
      Web.get
        ~headers:[ ("User-Agent", health_user_agent) ]
        ctx.web
        (Uri.of_string (health_base ^ "/?packages=dune&show-latest-only=true"))
    with
    | Ok (200, page) -> (run_of_page page, None)
    | Ok (s, _) -> (None, Some (Printf.sprintf "%s: HTTP %d" health_base s))
    | Error e -> (None, Some e)
  in
  let on_ocamlorg, err2 =
    match Actions.file gh ~repo:"ocaml/ocaml.org" ~path:"data/opam_health.yml" with
    | Ok text ->
        ( String.split_on_char '\n' text
          |> List.find_map (fun l ->
              if String.starts_with ~prefix:"run:" l then
                Some
                  ( String.trim (String.sub l 4 (String.length l - 4)) |> fun v ->
                    if String.length v >= 2 && v.[0] = '"' then
                      String.sub v 1 (String.length v - 2)
                    else v )
              else None),
          None )
    | Error e -> (None, Some e)
  in
  {
    latest;
    on_ocamlorg;
    health_deploy;
    health_error = (match err1 with Some e -> Some e | None -> err2);
  }

(* The manuals are cloned when ocaml.org's image is built: a commit is live
   once a build that started after it is deployed. *)
let manuals ~gh ~(ocamlorg : Repo_monitor.branch option) ~jobs =
  match Github.commit_info gh ~repo:manuals_repo ~ref_:"heads/main" with
  | Error e ->
      { commit = None; included = None; live_build = None; manuals_error = Some e }
  | Ok c ->
      let live_build =
        Option.bind ocamlorg (fun (b : Repo_monitor.branch) ->
            match b.deploys with
            | d :: _ ->
                Option.bind d.build_job (fun job ->
                    match Jobs.facts jobs job with
                    | Ok f -> Option.bind f.started Jobs.seconds_of_log_time
                    | Error _ -> None)
            | [] -> None)
      in
      let included =
        match (live_build, Ci.time_of_iso c.date) with
        | Some b, Some t -> Some (b >= t)
        | _ -> None
      in
      { commit = Some c; included; live_build; manuals_error = None }

let fetch ctx ~gh ~(ocamlorg : Repo_monitor.branch option) =
  let since = Unix.gettimeofday () -. (3. *. 86400.) in
  let jobs = Repo_monitor.jobs ctx.monitor in
  let (docs, health), manuals =
    Eio.Fiber.pair
      (fun () ->
        Eio.Fiber.pair (fun () -> docs ctx ~since) (fun () -> health ctx ~gh ~since))
      (fun () -> manuals ~gh ~ocamlorg ~jobs)
  in
  { docs; health; manuals }
