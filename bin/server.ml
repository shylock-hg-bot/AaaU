(** 服务器入口 *)

open Lwt.Syntax
open Cmdliner

let socket_path =
  let doc = "Unix socket path for client connections" in
  Arg.(value & opt string "/var/run/claude-bridge.sock" & info ["s"; "socket"] ~docv:"PATH" ~doc)

let shared_group =
  let doc = "Group name for authorized users" in
  Arg.(value & opt string "claude-shared" & info ["g"; "group"] ~docv:"GROUP" ~doc)

let agent_user =
  let doc = "System user for running agents" in
  Arg.(value & opt string "claude-agent" & info ["u"; "user"] ~docv:"USER" ~doc)

let log_dir =
  let doc = "Directory for audit logs" in
  Arg.(value & opt string "/var/log/claude-bridge" & info ["l"; "log-dir"] ~docv:"DIR" ~doc)

let daemonize =
  let doc = "Run as daemon" in
  Arg.(value & flag & info ["d"; "daemon"] ~doc)

let run_server socket_path shared_group agent_user log_dir daemonize =
  (* 初始化日志 *)
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);

  (* 守护进程化 *)
  if daemonize then begin
    (* 简化守护进程化 *)
    let pid = Unix.fork () in
    if pid > 0 then exit 0;
    Unix.setsid () |> ignore;
    Unix.close Unix.stdin;
    Unix.close Unix.stdout;
    Unix.close Unix.stderr
  end;

  (* 创建并启动服务器 *)
  let server = AaaU.Bridge.create
    ~socket_path
    ~shared_group
    ~agent_user
    ~log_dir
  in

  (* 信号处理 *)
  let handle_signal _sig =
    Lwt.async (fun () ->
      let* () = Logs_lwt.info (fun m -> m "Shutting down...") in
      AaaU.Bridge.stop server
    )
  in

  Sys.set_signal Sys.sigterm (Signal_handle handle_signal);
  Sys.set_signal Sys.sigint (Signal_handle handle_signal);

  (* 启动 *)
  Lwt_main.run (AaaU.Bridge.start server)

let cmd =
  let doc = "Agent-as-User PTY Bridge Server" in
  let info = Cmd.info "aaau-server" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(const run_server $ socket_path $ shared_group $ agent_user $ log_dir $ daemonize)

let () = exit (Cmd.eval cmd)
