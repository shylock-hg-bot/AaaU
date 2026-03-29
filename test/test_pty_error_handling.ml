(** Test PTY error handling - invalid slave path detection *)

open AaaU

let test_pty_error_handling () =
  (* This test verifies that the PTY module properly returns Error
     instead of raising exceptions for invalid states *)
  try
    match Pty.open_pty () with
    | Error msg ->
        (* An error message should be descriptive, not just "Failed to get ptsname" *)
        let has_descriptive_error =
          not (String.starts_with ~prefix:"Failed to get ptsname" msg) &&
          String.length msg > 0
        in
        if has_descriptive_error then
          Printf.printf "PASS: Got descriptive error: %s\n%!" msg
        else
          Printf.printf "FAIL: Error message not descriptive: %s\n%!" msg;
        has_descriptive_error
    | Ok (pty, slave) ->
        Printf.printf "PASS: PTY opened successfully with slave: %s\n%!" (slave :> string);
        let () = Lwt_main.run (Pty.close pty) in
        true
  with
  | Failure msg when String.starts_with ~prefix:"Failed to get ptsname" msg ->
      Printf.printf "FAIL: Unhandled Failure exception: %s\n%!" msg;
      false
  | e ->
      Printf.printf "FAIL: Unexpected exception: %s\n%!" (Printexc.to_string e);
      false

let () =
  Printf.printf "=== Test: PTY error handling ===\n%!";
  let result = test_pty_error_handling () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
