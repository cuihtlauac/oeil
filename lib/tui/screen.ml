(* Run an interactive display: keys in, a view redrawn at the bottom of the
   terminal, its last frame left in the scrollback. *)
open Console

let available () = Option.is_some (Console_eio.size Fmt.stderr)

(* [run ~env ~view ~final ~start ~on_key] draws [view ()] until [on_key]
   returns [`Quit]. [start ~sw ~dirty] starts the background work; it calls
   [dirty ()] when [view ()] changed. [final ()] is the last frame. *)
let run ~env ~view ~final ~start ~on_key =
  let clock = Eio.Stdenv.clock env in
  let size =
    match Console_eio.size Fmt.stderr with Some s -> s | None -> fun () -> (80, 24)
  in
  let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
  let t0 = Unix.gettimeofday () in
  let keys_run f = try Console_eio.Prompt.keys prompt f with Exit -> Ok () in
  let r =
    keys_run @@ fun input ->
    let d =
      Display.v ~size ~now:(fun () -> Unix.gettimeofday () -. t0) Fmt.stderr (view ())
    in
    let closed = ref false in
    let close () =
      if not !closed then begin
        closed := true;
        Display.set_view d (final ());
        Display.close d
      end
    in
    at_exit close;
    Fun.protect ~finally:close @@ fun () ->
    let dirty = ref true in
    let refresh () =
      dirty := false;
      Display.set_view d (view ())
    in
    Eio.Switch.run @@ fun sw ->
    (* Redraws: at once on a key, else ten times a second. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let rec loop () =
          if !dirty then refresh () else Display.redraw d;
          Eio.Time.sleep clock 0.1;
          loop ()
        in
        loop ());
    start ~sw ~dirty:(fun () -> dirty := true);
    let rec keys () =
      let page = max 1 (snd (size ()) - 6) in
      match Keys.read input with
      | exception End_of_file -> ()
      | k -> (
          let action = on_key ~page k in
          refresh ();
          match action with `Quit -> () | `Continue -> keys ())
    in
    keys ();
    (* Stop the background work. *)
    Eio.Switch.fail sw Exit
  in
  match r with
  | Ok () -> Ok ()
  | Error e -> Error (Fmt.str "%a" Console_eio.Prompt.pp_error e)
