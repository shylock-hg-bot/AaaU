(** 单个 Agent 会话管理 *)

type t

type client = {
  socket : Lwt_unix.file_descr;
  addr : string;
  user_info : Auth.user_info;
  connected_at : float;
  mutable last_activity : float;
}

val create :
  session_id:string ->
  creator:string ->
  audit:Audit.t ->
  (t, string) result Lwt.t
(** 创建新会话，启动 agent *)

val add_client :
  t ->
  socket:Lwt_unix.file_descr ->
  addr:string ->
  user_info:Auth.user_info ->
  (client, string) result Lwt.t
(** 添加客户端到会话 *)

val remove_client : t -> client -> unit Lwt.t
(** 移除客户端 *)

val handle_client_input :
  t ->
  client:client ->
  data:string ->
  unit Lwt.t
(** 处理客户端输入 *)

val is_alive : t -> bool
(** 检查 agent 是否存活 *)

val shutdown : t -> unit Lwt.t
(** 关闭会话 *)

val get_id : t -> string
val get_clients : t -> client list
val get_agent_pid : t -> int option

module Lwt_queue : sig
  type 'a t
  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit Lwt.t
  val pop : 'a t -> 'a Lwt.t
end
