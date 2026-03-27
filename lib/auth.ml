(** 认证实现 *)

type permission =
  | ReadOnly
  | Interactive
  | Admin

type user_info = {
  username : string;
  uid : int;
  gid : int;
  permission : permission;
}

let string_of_permission = function
  | ReadOnly -> "readonly"
  | Interactive -> "interactive"
  | Admin -> "admin"

let permission_of_string = function
  | "readonly" -> Some ReadOnly
  | "interactive" -> Some Interactive
  | "admin" -> Some Admin
  | _ -> None

let authenticate ~peer_uid ~peer_gid ~shared_group =
  try
    (* 获取用户信息 *)
    let user_entry = Unix.getpwuid peer_uid in
    let username = user_entry.Unix.pw_name in

    (* 检查是否在共享组中 *)
    let shared_gid = (Unix.getgrnam shared_group).Unix.gr_gid in

    let in_shared_group =
      peer_gid = shared_gid ||
      (* 获取用户的组列表 *)
      try
        (* 获取当前进程的所有组 *)
        let groups = Unix.getgroups () in
        Array.mem shared_gid groups
      with _ -> false
    in

    if not in_shared_group then
      Error (Printf.sprintf "User %s not in shared group %s" username shared_group)
    else
      (* 简单权限策略：uid 1000 以下为系统用户，给予 admin *)
      (* 实际应根据配置文件或数据库 *)
      let permission =
        if peer_uid < 1000 then Admin
        else Interactive
      in

      Ok {
        username;
        uid = peer_uid;
        gid = peer_gid;
        permission;
      }

  with
  | Not_found -> Error "User or group not found"
  | e -> Error (Printexc.to_string e)

let check_permission perm ~action =
  match perm, action with
  | Admin, _ -> true
  | Interactive, ("input" | "resize" | "ping") -> true
  | Interactive, _ -> false
  | ReadOnly, "read" -> true
  | ReadOnly, _ -> false
