(** 会话实现 *)

open Lwt.Syntax

let (>>=) = Lwt.(>>=)

module Lwt_queue = struct
  type 'a t = {
    queue : 'a Queue.t;
    condition : unit Lwt_condition.t;
    lock : Lwt_mutex.t;
    mutable item : 'a option;
  }

  let create () =
    {
      queue = Queue.create ();
      condition = Lwt_condition.create ();
      lock = Lwt_mutex.create ();
      item = None;
    }

  let push item t =
    Lwt_mutex.with_lock t.lock (fun () ->
      Queue.push item t.queue;
      Lwt_condition.signal t.condition ();
      Lwt.return_unit
    )

  let rec pop t =
    let* result = Lwt_mutex.with_lock t.lock (fun () ->
      if Queue.is_empty t.queue then
        Lwt.return_none
      else
        Lwt.return_some (Queue.pop t.queue)
    ) in
    match result with
    | Some item -> Lwt.return item
    | None ->
      let* () = Lwt_condition.wait t.condition in
      pop t
end

type client = {
  socket : Lwt_unix.file_descr;
  addr : string;
  user_info : Auth.user_info;
  connected_at : float;
  mutable last_activity : float;
}

type session = {
  session_id : string;
  creator : string;
  created_at : float;
  audit : Audit.t;

  (* PTY *)
  pty : Pty.t;
  agent_pid : int;

  (* 客户端管理 *)
  clients : (Lwt_unix.file_descr, client) Hashtbl.t;
  clients_lock : Lwt_mutex.t;

  (* 通信 *)
  input_queue : string Lwt_queue.t;
  output_buffer : Buffer.t;
  output_cond : unit Lwt_condition.t;

  (* 控制 *)
  mutable running : bool;
}

type t = session

let get_id s = s.session_id
let get_clients s = Hashtbl.fold (fun _ c acc -> c :: acc) s.clients []
let get_agent_pid s = Some s.agent_pid

(* 从 PTY 读取 agent 输出 *)
let rec pty_read_loop (t : session) =
  if not t.running then
    Lwt.return_unit
  else
    let buf = Bytes.create 4096 in
    let* n =
      Lwt.catch
        (fun () -> Pty.read t.pty buf 0 4096)
        (fun e ->
          let* () = Logs_lwt.err (fun m -> m "PTY read error: %s" (Printexc.to_string e)) in
          Lwt.return 0)
    in

    if n = 0 then (
      (* EOF - agent 退出 *)
      t.running <- false;
      Lwt_condition.broadcast t.output_cond ();
      let* () = Logs_lwt.info (fun m -> m "Agent EOF in session %s" t.session_id) in
      Lwt.return_unit
    ) else (
      let data = Bytes.sub_string buf 0 n in

      (* 保存到缓冲 *)
      Buffer.add_string t.output_buffer data;
      let () = 
        if Buffer.length t.output_buffer > 100000 then
          (* 保留后半部分 *)
          let content = Buffer.contents t.output_buffer in
          let keep = String.sub content (String.length content / 2) (String.length content / 2) in
          Buffer.reset t.output_buffer;
          Buffer.add_string t.output_buffer keep
      in

      (* 通知广播 *)
      Lwt_condition.broadcast t.output_cond ();

      (* 继续读取 *)
      pty_read_loop t
    )

(* 向 PTY 写入用户输入 *)
let rec pty_write_loop (t : session) =
  if not t.running then
    Lwt.return_unit
  else
    let* input = Lwt_queue.pop t.input_queue in
    let data = Bytes.of_string input in
    let* _ =
      Lwt.catch
        (fun () -> Pty.write t.pty data 0 (Bytes.length data))
        (fun e ->
          let* () = Logs_lwt.err (fun m -> m "PTY write error: %s" (Printexc.to_string e)) in
          Lwt.return 0)
    in
    pty_write_loop t

(* 广播输出到所有客户端 *)
let rec broadcast_loop (t : session) =
  if not t.running then
    Lwt.return_unit
  else
    let* () = Lwt_condition.wait t.output_cond in
    if not t.running then
      Lwt.return_unit
    else
      let data = Buffer.contents t.output_buffer in
      let* () =
        Lwt_mutex.with_lock t.clients_lock (fun () ->
          let dead = ref [] in
          
          let send_to_client fd client =
            Lwt.catch
              (fun () ->
                let bytes = Bytes.of_string data in
                Lwt_unix.write client.socket bytes 0 (String.length data) >>= fun _ ->
                Lwt.return_unit)
              (fun _ ->
                dead := fd :: !dead;
                Lwt.return_unit)
          in

          let* () =
            Hashtbl.fold
              (fun fd c acc -> let* () = send_to_client fd c in acc)
              t.clients
              Lwt.return_unit
          in

          (* 清理死客户端 *)
          List.iter (fun fd -> Hashtbl.remove t.clients fd) !dead;
          Lwt.return_unit
        )
      in
      broadcast_loop t

let create ~session_id ~creator ~audit =
  let* () = Logs_lwt.info (fun m -> m "Creating session %s" session_id) in

  (* 打开 PTY *)
  match Pty.open_pty () with
  | Error e -> Lwt.return_error e
  | Ok (pty, slave) ->
    (* 启动 agent *)
    let env = [
      "TERM", "xterm-256color";
      "CLAUDE_CODE_SESSION_ID", session_id;
      "PATH", "/usr/local/bin:/usr/bin:/bin";
    ] in

    match Pty.fork_agent
      ~slave
      ~user:"claude-agent"
      ~program:"/bin/bash"
      ~args:[]
      ~env
    with
    | Error e -> Lwt.return_error e
    | Ok pid ->
      let session = {
        session_id;
        creator;
        created_at = Unix.time ();
        audit;
        pty;
        agent_pid = pid;
        clients = Hashtbl.create 10;
        clients_lock = Lwt_mutex.create ();
        input_queue = Lwt_queue.create ();
        output_buffer = Buffer.create 102400;
        output_cond = Lwt_condition.create ();
        running = true;
      } in

      (* 审计记录 *)
      let* () = Audit.log audit {
        timestamp = Unix.time ();
        source = "system";
        user = creator;
        session_id;
        command_type = "session_start";
        content = Printf.sprintf "Agent PID %d" pid;
        metadata = ["pty", (Pty.get_slave_path pty :> string)];
      } in

      (* 启动处理循环 *)
      Lwt.async (fun () -> pty_read_loop session);
      Lwt.async (fun () -> pty_write_loop session);
      Lwt.async (fun () -> broadcast_loop session);

      Lwt.return_ok session

let add_client t ~socket ~addr ~user_info =
  Lwt_mutex.with_lock t.clients_lock (fun () ->
    if Hashtbl.length t.clients >= 10 then
      Lwt.return_error "Session full"
    else
      let client = {
        socket;
        addr;
        user_info;
        connected_at = Unix.time ();
        last_activity = Unix.time ();
      } in

      Hashtbl.add t.clients socket client;

      (* 发送历史输出 *)
      let history = Buffer.contents t.output_buffer in
      if String.length history > 0 then
        let* () =
          Lwt.catch
            (fun () -> 
              let bytes = Bytes.of_string history in
              Lwt_unix.write socket bytes 0 (String.length history) >>= fun _ ->
              Lwt.return_unit)
            (fun _ -> Lwt.return_unit)
        in
        Lwt.return_ok client
      else
        Lwt.return_ok client
  )

let remove_client t client =
  Lwt_mutex.with_lock t.clients_lock (fun () ->
    Hashtbl.remove t.clients client.socket;
    Lwt.return_unit
  )

(* 检查 agent 是否存活 *)
let is_alive t =
  try
    Unix.kill t.agent_pid 0;
    t.running
  with Unix.Unix_error _ -> false

let shutdown t =
  t.running <- false;
  Lwt_condition.broadcast t.output_cond ();

  (* 终止 agent *)
  (try Unix.kill t.agent_pid Sys.sigterm with _ -> ());
  let* () = Lwt_unix.sleep 1.0 in
  (try Unix.kill t.agent_pid Sys.sigkill with _ -> ());

  (* 关闭 PTY *)
  let* () = Pty.close t.pty in

  (* 断开所有客户端 *)
  let* () =
    Lwt_mutex.with_lock t.clients_lock (fun () ->
      Hashtbl.iter (fun fd _ ->
        try Unix.close (Lwt_unix.unix_file_descr fd) with _ -> ()) t.clients;
      Hashtbl.clear t.clients;
      Lwt.return_unit
    )
  in

  (* 刷审计日志 *)
  Audit.flush t.audit

let rec handle_client_input t ~client ~data =
  client.last_activity <- Unix.time ();

  let msg = Protocol.decode_client data in

  match msg with
  | Protocol.Input text ->
    (* 权限检查 *)
    (match client.user_info.Auth.permission with
     | Auth.ReadOnly ->
       let* () =
         Lwt.catch
           (fun () ->
             let err = Protocol.encode_server (Protocol.Error "Read-only mode") in
             let bytes = Bytes.of_string err in
             Lwt_unix.write client.socket bytes 0 (String.length err) >>= fun _ ->
             Lwt.return_unit)
           (fun _ -> Lwt.return_unit)
       in
       Lwt.return_unit

     | Auth.Interactive | Auth.Admin ->
       (* 审计记录 *)
       let* () = Audit.log t.audit {
         timestamp = Unix.time ();
         source = "human";
         user = client.user_info.Auth.username;
         session_id = t.session_id;
         command_type = "input";
         content = text;
         metadata = [];
       } in

       (* 加入输入队列 *)
       Lwt_queue.push text t.input_queue)

  | Protocol.Resize { rows; cols } ->
    Pty.set_terminal_size (Pty.fd t.pty) ~rows ~cols;
    Lwt.return_unit

  | Protocol.Ping ->
    let pong = Protocol.encode_server Protocol.Pong in
    let* () =
      Lwt.catch
        (fun () -> 
          let bytes = Bytes.of_string pong in
          Lwt_unix.write client.socket bytes 0 (String.length pong) >>= fun _ ->
          Lwt.return_unit)
        (fun _ -> Lwt.return_unit)
    in
    Lwt.return_unit

  | Protocol.GetStatus ->
    let status = `Assoc [
      "session_id", `String t.session_id;
      "clients", `Int (Hashtbl.length t.clients);
      "agent_alive", `Bool (is_alive t);
    ] in
    let resp = Protocol.encode_server (Protocol.Status status) in
    let* () =
      Lwt.catch
        (fun () -> 
          let bytes = Bytes.of_string resp in
          Lwt_unix.write client.socket bytes 0 (String.length resp) >>= fun _ ->
          Lwt.return_unit)
        (fun _ -> Lwt.return_unit)
    in
    Lwt.return_unit

  | Protocol.ForceKill ->
    (match client.user_info.Auth.permission with
     | Auth.Admin ->
       t.running <- false;
       Lwt_condition.broadcast t.output_cond ();
       let* () = shutdown t in
       Lwt.return_unit
     | _ ->
       let err = Protocol.encode_server (Protocol.Error "Permission denied") in
       let* () =
         Lwt.catch
           (fun () -> 
             let bytes = Bytes.of_string err in
             Lwt_unix.write client.socket bytes 0 (String.length err) >>= fun _ ->
             Lwt.return_unit)
           (fun _ -> Lwt.return_unit)
       in
       Lwt.return_unit)

  | Protocol.Unknown _ ->
    Lwt.return_unit
