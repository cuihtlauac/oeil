(* The workflows of a repository that run on their own (on a schedule) or
   open PRs (bots), and what their recent runs did. *)
open Oeil

let day_string t =
  match Ptime.of_float_s t with
  | Some p ->
      let y, m, d = Ptime.to_date p in
      Printf.sprintf "%04d-%02d-%02d" y m d
  | None -> "1970-01-01"

let fetch gh ~repo ~days =
  match Actions.workflows gh ~repo with
  | Error e -> Error e
  | Ok wfs ->
      let since = Unix.gettimeofday () -. (float_of_int days *. 86400.) in
      let errors = ref [] in
      let feeds =
        wfs
        |> List.filter (fun (w : Actions.workflow) ->
            w.wf_state = "active"
            && String.starts_with ~prefix:".github/workflows/" w.wf_path)
        |> Eio.Fiber.List.filter_map ~max_fibers:4 (fun (w : Actions.workflow) ->
            match Actions.file gh ~repo ~path:w.wf_path with
            | Error e ->
                (* A workflow whose file is not on the default branch (it
                   only ran from another branch): nothing to follow. *)
                if not (Job_log.contains e ~sub:"HTTP 404") then
                  errors := (w.wf_name ^ ": " ^ e) :: !errors;
                None
            | Ok source -> (
                let schedules = Cron.of_workflow source
                and branch = Ci.bot_branch source in
                if schedules = [] && branch = None then None
                else
                  match
                    Actions.runs gh ~repo ~workflow:w.wf_id ~since:(day_string since)
                  with
                  | Error e ->
                      errors := (w.wf_name ^ ": " ^ e) :: !errors;
                      None
                  | Ok runs ->
                      (* A bot's runs, whatever starts them; for a plain
                         scheduled workflow, only the runs on its own. *)
                      let runs =
                        List.filter
                          (fun (r : Ci.run) ->
                            match branch with
                            | Some _ -> r.event <> "pull_request"
                            | None ->
                                r.event = "schedule" || r.event = "workflow_dispatch")
                          runs
                      in
                      let prs =
                        match branch with
                        | None -> []
                        | Some b -> (
                            match Actions.bot_prs gh ~repo ~branch:b ~since with
                            | Ok l -> l
                            | Error e ->
                                errors := (w.wf_name ^ ": " ^ e) :: !errors;
                                [])
                      in
                      let produces_prs = Option.is_some branch in
                      Some
                        {
                          Ci.workflow_id = w.wf_id;
                          name = w.wf_name;
                          path = w.wf_path;
                          schedules;
                          branch;
                          runs =
                            List.map (fun r -> (r, Ci.outcome ~produces_prs ~prs r)) runs;
                          prs;
                        }))
      in
      Ok (feeds, List.rev !errors)
