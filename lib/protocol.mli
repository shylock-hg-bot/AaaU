(** 客户端-服务器通信协议 *)

type client_message =
  | Input of string           (** 普通输入 *)
  | Resize of { rows : int; cols : int }  (** 终端大小调整 *)
  | Ping                      (** 心跳 *)
  | GetStatus                 (** 查询状态 *)
  | ForceKill                 (** 强制终止（管理员） *)
  | Unknown of string

type server_message =
  | Output of string          (** PTY 输出 *)
  | Pong                      (** 心跳响应 *)
  | Status of Yojson.Safe.t   (** 会话状态 *)
  | Error of string           (** 错误消息 *)
  | Control of string         (** 控制通知 *)

val encode_client : client_message -> string
(** 编码客户端消息 *)

val decode_client : string -> client_message
(** 解码客户端消息 *)

val encode_server : server_message -> string
(** 编码服务器消息 *)

val decode_server : string -> server_message
(** 解码服务器消息 *)

val is_control : string -> bool
(** 检查是否为控制消息（以 \x01 开头） *)
