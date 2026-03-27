(** 服务器实现 *)

open Lwt.Syntax

type t = {
  socket_path : string;
  shared_group : string;
  agent_user : string;
  log_dir : string;

  mutable server_socket : Lwt_unix.file_descr option;
  audit : Audit.t;
  sessions : (string, Session.t) Hashtbl.t;
  sessions_lock : Lwt_mutex.t;
  mutable running : bool;
}

let create ~socket_path ~shared_group ~agent_user ~log_dir =
  let audit = Audit.create ~log_dir in
  {
    socket_path;
    shared_group;
    agent_user;
    log_dir;
    server_socket = None;
    audit;
    sessions = Hashtbl.create 100;
    sessions_lock = Lwt_mutex.create ();
    running = false;
  }

let setup_socket t =
  (* 清理旧 socket *)
  (try Unix.unlink t.socket_path with _ -> ());

  (* 创建目录 *)
  let dir = Filename.dirname t.socket_path in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error _ -> ());

  (* 创建 socket *)
  let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind socket (Unix.ADDR_UNIX t.socket_path) in
  Lwt_unix.listen socket 5;
  let* () = Lwt.return_unit in

  (* 设置权限 *)
  let gid = (Unix.getgrnam t.shared_group).Unix.gr_gid in
  Unix.chown t.socket_path 0 gid;
  Unix.chmod t.socket_path 0o660;

  t.server_socket <- Some socket;

  Logs_lwt.info (fun m -> m "Server listening on %s" t.socket_path)

let get_peer_credentials fd =
  (* 获取 Unix socket 对等方凭证 *)
  (* OCaml 没有直接的 SO_PEERCRED，需要通过 Unix 模块或 C 绑定 *)
  (* 这里使用假设的实现 *)
  try
    (* 在实际实现中，需要使用 getsockopt 获取 SO_PEERCRED *)
    (* 简化为从进程获取 *)
    let unix_fd = Lwt_unix.unix_file_descr fd in
    (* 使用 SO_PEERCRED 需要 C 绑定，这里简化处理 *)
    (* 在实际系统中应该调用 getsockopt SO_PEERCRED *)
    Ok (0, Unix.getuid (), Unix.getgid ())
  with e ->
    Error (Printexc.to_string e)

let authenticate_client t fd =
  match get_peer_credentials fd with
  | Error e -> Lwt.return_error e
  | Ok (_pid, uid, gid) ->
    Lwt.return (Auth.authenticate ~peer_uid:uid ~peer_gid:gid ~shared_group:t.shared_group)

let handle_handshake t client_fd =
  (* 读取握手消息 *)
  let buf = Bytes.create 1024 in
  let* n = Lwt_unix.read client_fd buf 0 1024 in

  if n = 0 then
    Lwt.return_error "Connection closed"
  else
    let msg = Bytes.sub_string buf 0 n in
    let msg = String.trim msg in

    if String.starts_with ~prefix:"SESSION:" msg then
      let session_id = String.sub msg 8 (String.length msg - 8) in
      Lwt_mutex.with_lock t.sessions_lock (fun () ->
        match Hashtbl.find_opt t.sessions session_id with
        | None -> Lwt.return_error (Printf.sprintf "Session %s not found" session_id)
        | Some session -> Lwt.return_ok (`Existing session)
      )
    else if msg = "NEW" then
      let session_id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string in
      let* result = Session.create ~session_id ~creator:"unknown" ~audit:t.audit in
      match result with
      | Error e -> Lwt.return_error e
      | Ok session ->
        let* () =
          Lwt_mutex.with_lock t.sessions_lock (fun () ->
            Hashtbl.add t.sessions session_id session;
            Lwt.return_unit
          )
        in
        let response = Printf.sprintf "SESSION:%s\n" session_id in
        let* _ = Lwt_unix.write_string client_fd response 0 (String.length response) in
        Lwt.return_ok (`New session)
    else
      Lwt.return_error "Invalid handshake. Use SESSION:id or NEW"

let handle_client t client_fd addr =
  let* () = Logs_lwt.info (fun m -> m "New connection from %s" addr) in

  (* 认证 *)
  let* auth_result = authenticate_client t client_fd in
  match auth_result with
  | Error e ->
    let* _ = Lwt_unix.write_string client_fd ("Auth failed: " ^ e ^ "\n") 0
        (String.length e + 12) in
    Lwt_unix.close client_fd

  | Ok user_info ->
    (* 握手 *)
    let* handshake = handle_handshake t client_fd in
    match handshake with
    | Error e ->
      let* _ = Lwt_unix.write_string client_fd (e ^ "\n") 0 (String.length e + 1) in
      Lwt_unix.close client_fd

    | Ok (`Existing session | `New session as result) ->
      let session =
        match result with
        | `Existing s -> s
        | `New s -> s
      in

      (* 添加客户端 *)
      let* client_result =
        Session.add_client session ~socket:client_fd ~addr ~user_info
      in
      match client_result with
      | Error e ->
        let* _ = Lwt_unix.write_string client_fd (e ^ "\n") 0 (String.length e + 1) in
        Lwt_unix.close client_fd

      | Ok client ->
        (* 主循环：转发客户端输入 *)
        let rec loop () =
          if not t.running then
            Lwt.return_unit
          else
            let buf = Bytes.create 4096 in
            let* n =
              Lwt.catch
                (fun () -> Lwt_unix.read client_fd buf 0 4096)
                (fun _ -> Lwt.return 0)
            in

            if n = 0 then
              (* 客户端断开 *)
              Session.remove_client session client
            else
              let data = Bytes.sub_string buf 0 n in
              let* () = Session.handle_client_input session ~client ~data in
              loop ()
        in

        let* () = loop () in
        Lwt_unix.close client_fd

let rec accept_loop t =
  if not t.running then
    Lwt.return_unit
  else
    match t.server_socket with
    | None -> Lwt.return_unit
    | Some socket ->
      let* client_fd, client_addr = Lwt_unix.accept socket in
      let addr =
        match client_addr with
        | Unix.ADDR_UNIX s -> s
        | _ -> "unknown"
      in

      (* 每个客户端一个独立协程 *)
      Lwt.async (fun () -> handle_client t client_fd addr);

      accept_loop t

let cleanup_sessions t =
  let rec loop () =
    let* () = Lwt_unix.sleep 60.0 in
    if not t.running then
      Lwt.return_unit
    else
      let* () =
        Lwt_mutex.with_lock t.sessions_lock (fun () ->
          let dead = ref [] in
          Hashtbl.iter (fun id session ->
            if not (Session.is_alive session) &&
               List.length (Session.get_clients session) = 0 then
              dead := (id, session) :: !dead
          ) t.sessions;

          List.iter (fun (id, session) ->
            Hashtbl.remove t.sessions id;
            Lwt.async (fun () -> Session.shutdown session)
          ) !dead;

          Lwt.return_unit
        )
      in
      loop ()
  in
  loop ()

let start t =
  let* () = setup_socket t in
  t.running <- true;

  (* 启动清理协程 *)
  Lwt.async (fun () -> cleanup_sessions t);

  (* 主 accept 循环 *)
  accept_loop t

let stop t =
  t.running <- false;

  (* 关闭 socket *)
  (match t.server_socket with
   | Some s -> Lwt.async (fun () -> Lwt_unix.close s)
   | None -> ());

  (* 关闭所有会话 *)
  let* () =
    Lwt_mutex.with_lock t.sessions_lock (fun () ->
      Hashtbl.fold (fun _ session acc ->
        let* () = Session.shutdown session in
        acc
      ) t.sessions Lwt.return_unit
    )
  in

  (* 刷审计日志 *)
  Audit.close t.audit
