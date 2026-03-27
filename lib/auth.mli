(** 认证与权限管理 *)

type permission =
  | ReadOnly      (** 只读查看 *)
  | Interactive   (** 可输入命令 *)
  | Admin         (** 可管理会话 *)

type user_info = {
  username : string;
  uid : int;
  gid : int;
  permission : permission;
}

val authenticate :
  peer_uid:int ->
  peer_gid:int ->
  shared_group:string ->
  (user_info, string) result
(** 根据 Unix socket 凭证认证用户 *)

val check_permission : permission -> action:string -> bool
(** 检查权限是否允许某操作 *)

val string_of_permission : permission -> string
val permission_of_string : string -> permission option
