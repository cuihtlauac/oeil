(* The life of a change, from what started it to production: a list of
   steps with times and durations. Pure. *)

type status = Done | Running | Failed | Pending | Waiting

type log =
  | Actions_job of int
  | Deployer_job of string
  | Ocurrent_job of { base : string; job : string }
      (** a job of another OCurrent service *)

type step = {
  key : string;
  time : float option;  (** when it started *)
  label : string;
  detail : string;
  status : status;
  duration : float option;
  log : log option;
  depth : int;  (** 0 for a step, 1 for a part of one *)
}

let step ?time ?(detail = "") ?duration ?log ?(depth = 0) ~key ~status label =
  { key; time; label; detail; status; duration; log; depth }

let status_of_conclusion = function
  | Some ("success" | "neutral" | "skipped") -> Done
  | Some _ -> Failed
  | None -> Running

let span a b = match (a, b) with Some a, Some b when b >= a -> Some (b -. a) | _ -> None

(* The deployer's stages as steps. *)
let of_stages ~now (stages : Tracker.stage list) ~time_of_since =
  List.map
    (fun (s : Tracker.stage) ->
      let time = Option.bind s.since time_of_since in
      let status, detail =
        match s.status with
        | Tracker.Done -> (Done, "")
        | Unchanged -> (Done, "image already live")
        | Running -> (Running, "")
        | Queued -> (Waiting, "queued")
        | Needs_confirmation -> (Waiting, "needs confirmation")
        | Pending -> (Pending, "")
        | Blocked -> (Pending, "blocked")
        | Failed m -> (Failed, m)
      in
      step ?time ~detail ~depth:1 ~key:("deploy:" ^ s.key) ~status
        ?duration:
          (match (status, time) with Running, Some t -> Some (now -. t) | _ -> None)
        ?log:(Option.map (fun j -> Deployer_job j) s.job)
        (Chain.kind_label s.kind))
    stages

(* Fill in each finished step's duration from the next step's start, where
   it has none. *)
let with_durations steps =
  let rec go = function
    | a :: (b :: _ as rest) ->
        let a =
          if a.duration = None && a.status = Done && a.depth = b.depth then
            { a with duration = span a.time b.time }
          else a
        in
        a :: go rest
    | l -> l
  in
  go steps

(* Where the time went, for a summary line: total, and the share spent
   waiting for a human (merge). *)
let summary steps =
  let times = List.filter_map (fun s -> s.time) steps in
  match times with
  | [] -> None
  | t :: _ ->
      let last =
        List.fold_left
          (fun acc s ->
            let e =
              match (s.time, s.duration) with
              | Some t, Some d -> t +. d
              | Some t, None -> t
              | _ -> acc
            in
            Float.max acc e)
          t steps
      in
      let first = List.fold_left Float.min t times in
      let waiting =
        List.fold_left
          (fun acc s ->
            if String.starts_with ~prefix:"merge:" s.key then
              acc +. Option.value s.duration ~default:0.
            else acc)
          0. steps
      in
      Some (last -. first, waiting)
