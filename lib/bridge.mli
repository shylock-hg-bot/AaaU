(** 主桥接服务器 *)

type t

val create :
  socket_path:string ->
  shared_group:string ->
  agent_user:string ->
  log_dir:string ->
  t
(** 创建服务器配置 *)

val start : t -> unit Lwt.t
(** 启动服务器，阻塞运行 *)

val stop : t -> unit Lwt.t
(** 停止服务器 *)
