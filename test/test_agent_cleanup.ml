(** Test agent process cleanup - verify no zombie processes *)

open AaaU

let test_agent_cleanup () =
  match Pty.open_pty () with
    | Error msg ->
        Printf.printf "FAIL: Could not open PTY: %s\n%!" msg;
        false
    | Ok (pty, _slave) ->
        (* Fork a child that exits immediately *)
        let pid = Unix.fork () in
        if pid = 0 then begin
          (* Child: exit immediately *)
          Unix._exit 42
        end else begin
          (* Parent: wait for child and verify it's reaped *)
          let (_, status) = Unix.waitpid [] pid in
          match status with
          | Unix.WEXITED 42 ->
              Printf.printf "PASS: Child process properly reaped (exit code 42)\n%!";
              Lwt_main.run (Pty.close pty);
              true
          | _ ->
              Printf.printf "FAIL: Unexpected child status\n%!";
              Lwt_main.run (Pty.close pty);
              false
        end

let () =
  Printf.printf "=== Test: Agent process cleanup ===\n%!";
  let result = test_agent_cleanup () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
