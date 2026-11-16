(* The repository summary: active PRs of each deployed branch. Pure. *)
open Oeil

type row = { branch : string; entry : Repo_state.entry }

type t = {
  repo : string;
  snapshot : Oeil_net.Repo_monitor.t option;
  selected : int;
  refreshing : bool;
  last_error : string option;
}

let v ~repo =
  { repo; snapshot = None; selected = 0; refreshing = true; last_error = None }

let rows t =
  match t.snapshot with
  | None -> []
  | Some s ->
      List.concat_map
        (fun (b : Oeil_net.Repo_monitor.branch) ->
          List.map (fun entry -> { branch = b.branch; entry }) b.entries)
        s.branches

let with_snapshot t s =
  let t = { t with snapshot = Some s; refreshing = false; last_error = None } in
  let n = List.length (rows t) in
  { t with selected = max 0 (min t.selected (n - 1)) }

type action = Nothing | Open of row | Refresh | Quit

let key t (k : Keys.t) =
  let n = List.length (rows t) in
  let select i = ({ t with selected = max 0 (min (n - 1) i) }, Nothing) in
  match k with
  | Up | Char 'k' -> select (t.selected - 1)
  | Down | Char 'j' -> select (t.selected + 1)
  | Home | Char 'g' -> select 0
  | End | Char 'G' -> select (n - 1)
  | Enter | Right | Char 'l' -> (
      match List.nth_opt (rows t) t.selected with
      | Some r -> (t, Open r)
      | None -> (t, Nothing))
  | Char 'r' -> ({ t with refreshing = true }, Refresh)
  | Char ('q' | 'Q') | Escape -> (t, Quit)
  | _ -> (t, Nothing)
