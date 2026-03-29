(** Test PTY open and close *)

open AaaU

let test_pty_open_close () =
  match Pty.open_pty () with
    | Error msg ->
        Printf.printf "FAIL: Could not open PTY: %s\n%!" msg;
        false
    | Ok (pty, slave) ->
        Printf.printf "Opened PTY with slave: %s\n%!" (slave :> string);
        (* Verify slave path format *)
        let valid_slave = String.starts_with ~prefix:"/dev/pts/" (slave :> string) in
        if not valid_slave then
          Printf.printf "FAIL: Invalid slave path format: %s\n%!" (slave :> string);
        (* Clean up *)
        let () = Lwt_main.run (Pty.close pty) in
        Printf.printf "Closed PTY successfully\n%!";
        valid_slave

let () =
  Printf.printf "=== Test: PTY open and close ===\n%!";
  let result = test_pty_open_close () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
