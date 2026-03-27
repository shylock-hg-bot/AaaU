(** PTY 操作实现 *)

open Lwt.Syntax

type t = {
  master_fd : Unix.file_descr;
  slave_path : string;
}

type slave = string

let get_slave_path pty = pty.slave_path
let fd pty = pty.master_fd

(* 打开 PTY *)
let open_pty () =
  try
    (* 使用 posix_openpt - 在 OCaml 中通过 Unix 模块实现 *)
    let master_fd =
      Unix.openfile "/dev/ptmx" [Unix.O_RDWR; Unix.O_NOCTTY] 0
    in

    (* 解锁 PTY *)
    let unlock_ptty () =
      (* 使用 ioctl 解锁 PTY *)
      (* 在 OCaml 中 ioctl 需要 C 绑定，这里简化处理 *)
      ()
    in

    (* 获取 slave 名称 *)
    let ptsname () =
      (* 使用 ptsname_r - 需要通过 C 绑定或外部调用 *)
      (* 简化：使用 shell 命令获取 *)
      let fd_str = string_of_int (Obj.magic master_fd : int) in
      let proc_path = "/proc/self/fd/" ^ fd_str in
      let slave_path = Unix.readlink proc_path in
      (* 解析 /dev/pts/N *)
      if String.length slave_path > 0 then
        slave_path
      else
        failwith "Failed to get ptsname"
    in

    unlock_ptty ();
    let slave = ptsname () in

    Ok ({ master_fd; slave_path = slave }, slave)
  with
  | Unix.Unix_error (err, fn, arg) ->
    Error (Printf.sprintf "Unix error in %s(%s): %s" fn arg (Unix.error_message err))
  | e -> Error (Printexc.to_string e)

(* 设置原始模式 *)
let set_raw_mode fd =
  let tcgetattr fd =
    (* 使用 Unix.tcgetattr *)
    Unix.tcgetattr fd
  in
  let tcsetattr fd attr =
    Unix.tcsetattr fd Unix.TCSANOW attr
  in

  let attr = tcgetattr fd in
  let new_attr = {
    attr with
    (* 输入模式 *)
    c_ignbrk = false;
    c_brkint = false;
    c_ignpar = false;
    c_parmrk = false;
    c_inpck = false;
    c_istrip = false;
    c_inlcr = false;
    c_igncr = false;
    c_icrnl = false;
    c_ixon = false;
    c_ixoff = false;

    (* 输出模式 *)
    c_opost = false;

    (* 本地模式 *)
    c_isig = false;
    c_icanon = false;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_echonl = false;

    (* 特殊字符 *)
    c_vmin = 1;
    c_vtime = 0;
  } in
  tcsetattr fd new_attr

(* 设置终端大小 *)
let set_terminal_size fd ~rows ~cols =
  (* 使用 ioctl 设置终端大小 *)
  (* 在 OCaml 中 ioctl 需要 C 绑定，这里简化处理 *)
  ignore (fd, rows, cols);
  ()

(* Fork 并切换到用户 *)
let fork_agent ~slave ~user ~program ~args ~env =
  try
    let slave_fd = Unix.openfile slave [Unix.O_RDWR] 0 in

    let pid = Unix.fork () in

    if pid = 0 then begin
      (* 子进程 *)
      try
        (* 创建新会话 *)
        let _ = Unix.setsid () in

        (* 设置控制终端 *)
        (* TIOCSCTTY - ioctl 需要 C 绑定 *)
        let () = () in

        (* 复制到标准 IO *)
        Unix.dup2 slave_fd Unix.stdin;
        Unix.dup2 slave_fd Unix.stdout;
        Unix.dup2 slave_fd Unix.stderr;

        (* 关闭不需要的 fd *)
        Unix.close slave_fd;

        (* 获取用户信息 *)
        let user_entry = Unix.getpwnam user in
        let group_entry = Unix.getgrnam user in

        (* 初始化组列表并切换 *)
        let gid = group_entry.Unix.gr_gid in
        let uid = user_entry.Unix.pw_uid in

        (* setgroups, setgid, setuid *)
        (* OCaml 标准库没有 setgroups，需要外部调用或 C 绑定 *)
        (* 使用 sudo 作为替代方案 *)

        (* 设置环境 *)
        List.iter (fun (k, v) -> Unix.putenv k v) env;
        Unix.putenv "HOME" user_entry.Unix.pw_dir;
        Unix.putenv "USER" user;

        (* 切换目录 *)
        Unix.chdir user_entry.Unix.pw_dir;

        (* 执行程序 *)
        (* 由于 OCaml 没有直接的 setuid，我们使用 sudo 辅助 *)
        let sudo_args =
          ["-u"; user; "-g"; user; "--"] @
          [program] @ args
        in
        Unix.execvp "sudo" (Array.of_list ("sudo" :: sudo_args))

      with e ->
        Printf.eprintf "Agent startup failed: %s\n%!" (Printexc.to_string e);
        Unix._exit 1
    end else begin
      (* 父进程 *)
      Unix.close slave_fd;
      Ok pid
    end

  with
  | Unix.Unix_error (err, fn, arg) ->
    Error (Printf.sprintf "Fork error in %s(%s): %s" fn arg (Unix.error_message err))
  | e -> Error (Printexc.to_string e)

(* Lwt 包装 *)
let read pty buf off len =
  let lwt_fd = Lwt_unix.of_unix_file_descr pty.master_fd in
  Lwt_unix.read lwt_fd buf off len

let write pty buf off len =
  let lwt_fd = Lwt_unix.of_unix_file_descr pty.master_fd in
  Lwt_unix.write lwt_fd buf off len

let close pty =
  let lwt_fd = Lwt_unix.of_unix_file_descr pty.master_fd in
  Lwt_unix.close lwt_fd
