let src = Logs.Src.create "oeil.web" ~doc:"oeil HTTP client"

module Log = (val Logs.src_log src)

type t = { client : Cohttp_eio.Client.t }

let user_agent = "oeil/0.1 (+https://github.com/cuihtlauac/oeil)"
let rng_initialised = ref false

let make net =
  if not !rng_initialised then begin
    Mirage_crypto_rng_unix.use_default ();
    rng_initialised := true
  end;
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg m) -> failwith ("oeil: cannot load CA certificates: " ^ m)
  in
  let config =
    match Tls.Config.client ~authenticator () with
    | Ok c -> c
    | Error (`Msg m) -> failwith ("oeil: TLS configuration: " ^ m)
  in
  let https uri raw =
    let host =
      Option.bind (Uri.host uri) (fun h ->
          match Domain_name.of_string h with
          | Ok d -> Result.to_option (Domain_name.host d)
          | Error _ -> None)
    in
    Tls_eio.client_of_flow ?host config raw
  in
  { client = Cohttp_eio.Client.make ~https:(Some https) net }

let headers_of extra = Http.Header.of_list (("User-Agent", user_agent) :: extra)

let error_of_exn uri e =
  Error (Printf.sprintf "%s: %s" (Uri.to_string uri) (Printexc.to_string e))

let get ?(headers = []) t uri =
  Log.debug (fun m -> m "GET %a" Uri.pp uri);
  match
    Eio.Switch.run @@ fun sw ->
    let resp, body =
      Cohttp_eio.Client.get t.client ~sw ~headers:(headers_of headers) uri
    in
    let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:(64 * 1024 * 1024) in
    (Http.Status.to_int (Http.Response.status resp), body)
  with
  | r -> Ok r
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e -> error_of_exn uri e

let stream ?(headers = []) t uri ~on_chunk =
  Log.debug (fun m -> m "GET (stream) %a" Uri.pp uri);
  match
    Eio.Switch.run @@ fun sw ->
    let resp, body =
      Cohttp_eio.Client.get t.client ~sw ~headers:(headers_of headers) uri
    in
    let status = Http.Status.to_int (Http.Response.status resp) in
    if status <> 200 then status
    else begin
      let buf = Cstruct.create 16384 in
      let rec loop () =
        match Eio.Flow.single_read body buf with
        | n -> if on_chunk (Cstruct.to_string ~len:n buf) = `Continue then loop ()
        | exception End_of_file -> ()
      in
      loop ();
      status
    end
  with
  | 200 -> Ok ()
  | s -> Error (Printf.sprintf "%s: HTTP %d" (Uri.to_string uri) s)
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e -> error_of_exn uri e
