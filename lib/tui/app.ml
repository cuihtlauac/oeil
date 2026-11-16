(* oeil watch on a terminal: one target's deployment. *)
open Oeil
open Oeil_net

type result = [ `Deployed | `Failed of string | `Timeout | `Quit | `Error of string ]

let run ~env ~web ~instance ~github ~(target : Target.t) ?branch ~interval ~timeout () :
    result =
  let clock = Eio.Stdenv.clock env in
  let inst = Instance.make web instance in
  let screen = ref None in
  let resolve_error = ref None in
  let get () = Option.get !screen in
  let start ~sw ~dirty =
    let w = Watch_screen.create ~sw ~inst ~title:(Target.to_string target) ~dirty in
    screen := Some w;
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let rec resolve () =
          match Resolve.resolve ~github ?branch target with
          | Error e ->
              resolve_error := Some e;
              Watch_screen.set w (fun m -> { m with phase = Error e });
              None
          | Ok (`Ready r) -> Some r
          | Ok (`Not_merged (pr : Github.pull)) ->
              Watch_screen.set w (fun m ->
                  {
                    m with
                    phase =
                      Waiting_merge
                        (Printf.sprintf "waiting for #%d to be merged" pr.number);
                  });
              Eio.Time.sleep clock (Float.max interval 30.);
              resolve ()
        in
        (match resolve () with
        | None -> ()
        | Some r ->
            Watch_screen.monitor w ~clock ~github ~target:r.target ~title:r.title
              ~interval ~timeout);
        `Stop_daemon)
  in
  (* Before [start] runs, draw an empty model. *)
  let blank = Model.v ~title:(Target.to_string target) ~now:(Unix.gettimeofday ()) in
  let view () =
    match !screen with
    | Some w -> Watch_screen.view w
    | None -> Render.view ~logs:(Log_store.create ()) blank
  in
  let final () =
    match !screen with
    | Some w -> Watch_screen.final w
    | None -> Render.final ~now:(Unix.gettimeofday ()) blank
  in
  match
    Screen.run ~env ~view ~final ~start ~on_key:(fun ~page k ->
        Watch_screen.key (get ()) ~page k)
  with
  | Error e -> `Error e
  | Ok () -> (
      match (!resolve_error, Option.bind !screen Watch_screen.outcome) with
      | Some e, _ -> `Error e
      | None, Some o -> (o :> result)
      | None, None -> `Quit)
