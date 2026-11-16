(* The deploy history of a branch, newest first, read back from the job
   histories of its service, push-manifest and build nodes. *)
open Oeil

let succeeded (f : Tracker.Facts.t) = f.outcome = `Succeeded

(* Deploys newest first: all those since [since] (seconds since the epoch),
   and the one before, from which they changed things. *)
let history jobs ~repo ~branch ~(stages : Chain.stage list) ~since :
    Repo_state.deploy list =
  let first kind = List.find_opt (fun (s : Chain.stage) -> kind s.kind) stages in
  let service = first (function Chain.Service _ -> true | _ -> false) in
  let push = first (function Chain.Push _ -> true | _ -> false) in
  let build = first (function Chain.Build _ -> true | _ -> false) in
  (* A node shows no job while its input is rebuilt: then find its latest
     run in the query page, by what its log says it works on. *)
  let job_of (s : Chain.stage) ~op ~needle =
    match s.node.job with
    | Some j -> Some j
    | None -> Jobs.latest jobs ~op ~matches:(fun h -> Job_log.contains h ~sub:needle) ()
  in
  let service_job =
    Option.bind service (fun (s : Chain.stage) ->
        match s.kind with
        | Chain.Service name -> job_of s ~op:"docker-service" ~needle:("\"" ^ name ^ "\"")
        | _ -> None)
  in
  match service_job with
  | None -> []
  | Some sjob ->
      (* Successful service updates and the image each deployed. *)
      let runs = Jobs.runs jobs sjob in
      Jobs.prefetch_facts jobs runs;
      let updates =
        runs
        |> List.filter_map (fun job ->
            match Jobs.facts jobs job with
            | Ok f when succeeded f -> (
                match f.inputs with d :: _ -> Some (job, d) | [] -> None)
            | _ -> None)
      in
      (* An image is live from the oldest update of a run of updates with
         the same image. *)
      let rec collapse = function
        | [] -> []
        | (job, d) :: rest ->
            let rec oldest job = function
              | (j, d') :: r when d' = d -> oldest j r
              | r -> (job, r)
            in
            let job, rest = oldest job rest in
            (job, d) :: collapse rest
      in
      let recent (job, _) =
        match Jobs.seconds_of_key (Jobs.time_key job) with
        | Some t -> t >= since
        | None -> true
      in
      let updates =
        let rec take = function
          | x :: rest when recent x -> x :: take rest
          | x :: _ -> [ x ]
          | [] -> []
        in
        take (collapse updates)
      in
      let push_runs =
        match
          Option.bind push (fun (s : Chain.stage) ->
              match s.kind with
              | Chain.Push tag ->
                  job_of s ~op:"docker-push-manifest" ~needle:("push " ^ tag ^ " ")
              | _ -> None)
        with
        | Some j -> Jobs.runs jobs j
        | None -> []
      in
      Jobs.prefetch_facts jobs push_runs;
      (* The push-manifest job that published [digest]: its inputs are the
         built images. *)
      let built_from digest =
        List.find_map
          (fun job ->
            match Jobs.facts jobs job with
            | Ok f when succeeded f && f.output = Some digest -> Some (job, f.inputs)
            | _ -> None)
          push_runs
      in
      let hint =
        Option.bind build (fun (s : Chain.stage) ->
            if State.is_active (Svg_graph.effective_state s.node) then None
            else s.node.job)
      in
      Eio.Fiber.List.map ~max_fibers:4
        (fun (at, digest) ->
          match built_from digest with
          | None ->
              { Repo_state.at; digest; commit = None; push_job = None; build_job = None }
          | Some (pjob, built) ->
              let found =
                Jobs.commit_of_digest jobs ~repo ~branch ~hint
                  ~before:(Jobs.time_key pjob) built
              in
              {
                Repo_state.at;
                digest;
                commit = Option.map fst found;
                push_job = Some pjob;
                build_job = Option.map snd found;
              })
        updates

(* The stages of a past deploy, for showing it: its build and push jobs, and
   the pull and service jobs that took its image. *)
let stages_of jobs ~(stages : Chain.stage list) (d : Repo_state.deploy) :
    Tracker.stage list =
  let done_ (s : Chain.stage) job =
    let since =
      Option.bind job (fun j ->
          match Jobs.facts jobs j with Ok f -> f.started | Error _ -> None)
    in
    {
      Tracker.key = s.key;
      kind = s.kind;
      status = (if job = None then Pending else Done);
      job;
      since;
    }
  in
  (* The run of a pull or service node that used [d.digest]: among the
     node's runs, or, when the node shows no job, in the query page by its
     header, which names the image (and the service). *)
  let run_with_image (s : Chain.stage) ~op ~also =
    let uses_image (f : Tracker.Facts.t) =
      f.outcome = `Succeeded && List.mem d.digest f.inputs
    in
    match s.node.job with
    | Some j ->
        let runs = Jobs.runs jobs j in
        Jobs.prefetch_facts jobs runs;
        List.find_opt
          (fun r ->
            match Jobs.facts jobs r with Ok f -> uses_image f | Error _ -> false)
          (List.rev runs)
    | None ->
        Jobs.latest jobs ~op
          ~matches:(fun h ->
            Job_log.contains h ~sub:d.digest && Job_log.contains h ~sub:also)
          ()
  in
  Eio.Fiber.List.map ~max_fibers:4
    (fun (s : Chain.stage) ->
      match s.kind with
      | Chain.Build _ -> done_ s d.build_job
      | Chain.Push _ -> done_ s d.push_job
      | Chain.Pull _ -> done_ s (run_with_image s ~op:"docker-pull" ~also:"pull")
      | Chain.Service name ->
          done_ s (run_with_image s ~op:"docker-service" ~also:("\"" ^ name ^ "\""))
      | Chain.Notify ->
          {
            Tracker.key = s.key;
            kind = s.kind;
            status = Pending;
            job = None;
            since = None;
          })
    stages
  |> List.filter (fun (st : Tracker.stage) -> st.kind <> Chain.Notify)
