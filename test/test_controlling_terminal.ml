(** Test controlling terminal setup *)

open AaaU

let test_controlling_terminal () =
  match Pty.open_pty () with
    | Error msg ->
        Printf.printf "FAIL: Could not open PTY: %s\n%!" msg;
        false
    | Ok (pty, slave) ->
        (* Fork a child to test controlling terminal setup *)
        let pid = Unix.fork () in
        if pid = 0 then begin
          (* Child: create new session and set controlling terminal *)
          let _ = Unix.setsid () in
          let slave_fd = Unix.openfile (slave :> string) [Unix.O_RDWR] 0 in
          (* This should succeed - setting controlling terminal *)
          Pty.set_controlling_terminal slave_fd;
          Unix.close slave_fd;
          Unix._exit 0
        end else begin
          (* Parent: wait for child *)
          let (_, status) = Unix.waitpid [] pid in
          Lwt_main.run (Pty.close pty);
          match status with
          | Unix.WEXITED 0 ->
              Printf.printf "PASS: Controlling terminal set successfully\n%!";
              true
          | _ ->
              Printf.printf "FAIL: Child process failed to set controlling terminal\n%!";
              false
        end

let () =
  Printf.printf "=== Test: Controlling terminal (TIOCSCTTY) ===\n%!";
  let result = test_controlling_terminal () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
