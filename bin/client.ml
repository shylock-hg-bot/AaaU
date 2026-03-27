(** 客户端入口 *)

open Lwt.Syntax
open Cmdliner

let socket_path =
  let doc = "Server socket path" in
  Arg.(value & opt string "/var/run/claude-bridge.sock" & info ["s"; "socket"] ~docv:"PATH" ~doc)

let session_id =
  let doc = "Existing session ID to join" in
  Arg.(value & opt (some string) None & info ["n"; "session"] ~docv:"ID" ~doc)

let readonly =
  let doc = "Read-only mode" in
  Arg.(value & flag & info ["r"; "readonly"] ~doc)

let rec run_client_lwt socket_path session_id readonly =
  (* 连接服务器 *)
  let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (Unix.ADDR_UNIX socket_path) in

  (* 发送握手 *)
  let handshake =
    match session_id with
    | Some id -> "SESSION:" ^ id
    | None -> "NEW"
  in
  let* _ = Lwt_unix.write_string socket handshake 0 (String.length handshake) in

  (* 读取响应 *)
  let buf = Bytes.create 1024 in
  let* n = Lwt_unix.read socket buf 0 1024 in
  let response = Bytes.sub_string buf 0 n in
  print_endline response;

  if String.starts_with ~prefix:"SESSION:" response then begin
    (* 成功连接，进入交互模式 *)
    if readonly then
      run_readonly socket
    else
      run_interactive socket
  end else
    Lwt.return_unit

and run_readonly socket =
  (* 只读模式：只接收，不发送 *)
  let rec loop () =
    let buf = Bytes.create 4096 in
    let* n = Lwt_unix.read socket buf 0 4096 in
    if n = 0 then
      Lwt.return_unit
    else
      let data = Bytes.sub_string buf 0 n in
      if not (AaaU.Protocol.is_control data) then
        print_string data;
      loop ()
  in
  loop ()

and run_interactive socket =
  (* 交互模式：同时处理 stdin 和 socket *)
  (* 设置终端为原始模式 *)
  let old_tty = Unix.tcgetattr Unix.stdin in
  let new_tty = { old_tty with
    c_icanon = false;
    c_echo = false;
    c_vmin = 1;
    c_vtime = 0;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW new_tty;

  (* 发送初始窗口大小 *)
  let (rows, cols) = get_terminal_size () in
  let resize_msg = AaaU.Protocol.encode_client
    (Resize { rows; cols })
  in
  let* _ = Lwt_unix.write_string socket resize_msg 0 (String.length resize_msg) in

  (* 启动两个协程 *)
  let stdin_to_socket () =
    let rec loop () =
      let buf = Bytes.create 1024 in
      let n = Unix.read (Unix.descr_of_in_channel stdin) buf 0 1024 in
      if n = 0 then
        Lwt.return_unit
      else
        let data = Bytes.sub_string buf 0 n in
        let* _ = Lwt_unix.write_string socket data 0 n in
        loop ()
    in
    loop ()
  in

  let socket_to_stdout () =
    let rec loop () =
      let buf = Bytes.create 4096 in
      let* n = Lwt_unix.read socket buf 0 4096 in
      if n = 0 then
        Lwt.return_unit
      else
        let data = Bytes.sub_string buf 0 n in
        print_string data;
        flush stdout;
        loop ()
    in
    loop ()
  in

  (* 恢复终端 *)
  let cleanup () =
    Unix.tcsetattr Unix.stdin Unix.TCSANOW old_tty
  in

  try
    let* () = Lwt.pick [stdin_to_socket (); socket_to_stdout ()] in
    cleanup ();
    Lwt.return_unit
  with e ->
    cleanup ();
    raise e

and get_terminal_size () =
  (* 使用 TIOCGWINSZ - ioctl 需要 C 绑定，使用默认值 *)
  (24, 80)

let cmd =
  let doc = "Agent-as-User Bridge Client" in
  let info = Cmd.info "aaau-client" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(const (fun a b c -> Lwt_main.run (run_client_lwt a b c)) $ socket_path $ session_id $ readonly)

let () = exit (Cmd.eval cmd)
