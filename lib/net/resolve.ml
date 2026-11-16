open Oeil

type t = { target : Tracker.target; title : string }

let short sha = if String.length sha > 9 then String.sub sha 0 9 else sha

(* [Ok (`Ready t)], or [Ok (`Not_merged pr)] for an open pull request. *)
let resolve ?github ?branch (target : Target.t) =
  match target with
  | Branch { repo; branch = b } ->
      let branch = Option.value branch ~default:b in
      Ok
        (`Ready
           {
             target = { repo; branch; sha = None };
             title = Printf.sprintf "%s %s (latest)" repo branch;
           })
  | Commit { repo; sha } ->
      let branch = Option.value branch ~default:"main" in
      Ok
        (`Ready
           {
             target = { repo; branch; sha = Some sha };
             title = Printf.sprintf "%s %s @ %s" repo branch (short sha);
           })
  | Pr { repo; number } -> (
      match github with
      | None -> Error "following a pull request needs GitHub access"
      | Some gh -> (
          match Github.pull gh ~repo ~number with
          | Error _ as e -> e
          | Ok pr when pr.merged -> (
              match pr.merge_commit with
              | None ->
                  Error
                    (Printf.sprintf "%s#%d is merged but has no merge commit" repo number)
              | Some sha ->
                  let branch = Option.value branch ~default:pr.base in
                  Ok
                    (`Ready
                       {
                         target = { repo; branch; sha = Some sha };
                         title =
                           Printf.sprintf "%s#%d %s → %s @ %s" repo number pr.title branch
                             (short sha);
                       }))
          | Ok pr when pr.state = "closed" ->
              Error (Printf.sprintf "%s#%d was closed without being merged" repo number)
          | Ok pr -> Ok (`Not_merged pr)))
