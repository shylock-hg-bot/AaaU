(** 审计日志系统 *)

type record = {
  timestamp : float;
  source : string;           (** "human", "agent", "system" *)
  user : string;
  session_id : string;
  command_type : string;     (** "input", "output", "control", "session_start" *)
  content : string;
  metadata : (string * string) list;
}

type t

val create : log_dir:string -> t
(** 创建审计日志器 *)

val log : t -> record -> unit Lwt.t
(** 记录一条日志 *)

val flush : t -> unit Lwt.t
(** 强制刷盘 *)

val close : t -> unit Lwt.t
(** 关闭日志 *)
