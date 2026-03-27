(** PTY (Pseudo Terminal) 操作接口 *)

type t
(** PTY master 文件描述符封装 *)

type slave = private string
(** PTY slave 设备路径 *)

val open_pty : unit -> (t * slave, string) result
(** 打开新的 PTY master/slave 对 *)

val set_raw_mode : Unix.file_descr -> unit
(** 设置原始终端模式 *)

val set_terminal_size : Unix.file_descr -> rows:int -> cols:int -> unit
(** 设置终端窗口大小 (TIOCSWINSZ) *)

val fork_agent :
  slave:slave ->
  user:string ->
  program:string ->
  args:string list ->
  env:(string * string) list ->
  (int, string) result
(**
  Fork 子进程，切换到指定用户，在 PTY slave 中执行程序。
  返回子进程 PID。
*)

val read : t -> bytes -> int -> int -> int Lwt.t
(** 从 PTY master 读取数据 *)

val write : t -> bytes -> int -> int -> int Lwt.t
(** 向 PTY master 写入数据 *)

val close : t -> unit Lwt.t
(** 关闭 PTY master *)

val fd : t -> Unix.file_descr
(** 获取底层文件描述符（用于 select/poll） *)

val get_slave_path : t -> slave
(** 获取 slave 路径 *)
