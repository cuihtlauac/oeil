(* GitHub Actions runs, and what a run of a PR-producing workflow did. Pure. *)

type run = {
  id : int;
  event : string;  (** "schedule", "push", "workflow_dispatch", … *)
  status : string;  (** "queued", "in_progress", "completed" *)
  conclusion : string option;  (** "success", "failure", "cancelled", … *)
  started : float;
  finished : float option;  (** when completed *)
  head_sha : string;
  url : string;
}

type job = {
  job_id : int;
  name : string;
  job_conclusion : string option;
  job_started : float option;
  job_finished : float option;
  failed_step : string option;
}

(* A PR opened by a bot workflow on its branch. *)
type bot_pr = {
  number : int;
  created : float;
  merged : float option;
  closed : bool;  (** closed without being merged *)
  commit_dates : float list;  (** when its commits were made *)
}

type outcome =
  | In_progress
  | Failed of string  (** conclusion *)
  | No_change  (** a PR-producing run that opened or updated nothing *)
  | Opened of int
  | Updated of int
  | Succeeded  (** a run of a workflow that produces no PR *)

(* What a completed run did, from timestamps: a PR created while it ran was
   opened by it; one that got a commit while it ran was updated by it. *)
let outcome ~produces_prs ~(prs : bot_pr list) (r : run) =
  if r.status <> "completed" then In_progress
  else
    match r.conclusion with
    | Some ("success" | "neutral" | "skipped") when not produces_prs -> Succeeded
    | Some ("success" | "neutral" | "skipped") -> (
        let stop = Option.value r.finished ~default:r.started +. 120. in
        let within t = t >= r.started -. 60. && t <= stop in
        match List.find_opt (fun p -> within p.created) prs with
        | Some p -> Opened p.number
        | None -> (
            match List.find_opt (fun p -> List.exists within p.commit_dates) prs with
            | Some p -> Updated p.number
            | None -> No_change))
    | Some c -> Failed c
    | None -> Failed "unknown"

let outcome_to_string = function
  | In_progress -> "running"
  | Failed "failure" -> "failed"
  | Failed c -> "failed (" ^ c ^ ")"
  | No_change -> "no change"
  | Opened n -> Printf.sprintf "opened #%d" n
  | Updated n -> Printf.sprintf "updated #%d" n
  | Succeeded -> "succeeded"

type feed = {
  workflow_id : int;
  name : string;
  path : string;
  schedules : Cron.t list;
  branch : string option;  (** the bot branch its PRs come from *)
  runs : (run * outcome) list;  (** newest first *)
  prs : bot_pr list;  (** PRs from [branch], newest first *)
}

let produces_prs f = Option.is_some f.branch

type cell = Pr | Changeless | Fail | Ok_run | No_run

(* One cell per UTC day, oldest first, ending today: what the feed's runs
   did that day (a failure shows over a PR, a PR over no change). *)
let strip ~days ~now f =
  let day t = Float.to_int (t /. 86400.) in
  let today = day now in
  List.init days (fun i ->
      let d = today - (days - 1 - i) in
      let outs =
        List.filter_map (fun (r, o) -> if day r.started = d then Some o else None) f.runs
      in
      if List.exists (function Failed _ -> true | _ -> false) outs then Fail
      else if List.exists (function Opened _ | Updated _ -> true | _ -> false) outs then
        Pr
      else if List.mem Succeeded outs then Ok_run
      else if List.mem No_change outs || List.mem In_progress outs then Changeless
      else No_run)

(* The latest PR a run of the feed opened or updated. *)
let last_pr f =
  List.find_map
    (fun (_, o) -> match o with Opened n | Updated n -> Some n | _ -> None)
    f.runs

(* The bot branch named in a workflow file: "branch: bot/planet", or an
   environment variable "BRANCH: bot/attribution" it refers to. *)
let bot_branch source =
  let lines = String.split_on_char '\n' source |> List.map String.trim in
  let value key l =
    if String.starts_with ~prefix:key l then
      let v =
        String.trim
          (String.sub l (String.length key) (String.length l - String.length key))
      in
      let v =
        if String.length v >= 2 && (v.[0] = '\'' || v.[0] = '"') then
          String.sub v 1 (String.length v - 2)
        else v
      in
      if v = "" || String.starts_with ~prefix:"${{" v then None else Some v
    else None
  in
  if not (List.exists (fun l -> Job_log.contains l ~sub:"create-pull-request") lines) then
    None
  else
    match List.find_map (value "branch:") lines with
    | Some b -> Some b
    | None -> List.find_map (value "BRANCH:") lines

(* "2026-11-16T09:23:48Z" -> seconds since the epoch. *)
let time_of_iso s =
  try
    Scanf.sscanf s "%4d-%2d-%2dT%2d:%2d:%2d" (fun y mo d h mi se ->
        Option.map Ptime.to_float_s (Ptime.of_date_time ((y, mo, d), ((h, mi, se), 0))))
  with _ -> None
