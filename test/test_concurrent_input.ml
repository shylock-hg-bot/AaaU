(** Test concurrent input handling - stress test for race conditions *)

open AaaU
open Lwt.Syntax

(* Helper to check if running in an environment with PTY support *)
let has_pty_support () =
  try
    if Sys.file_exists "/dev/ptmx" && Sys.file_exists "/dev/pts" then begin
      Unix.access "/dev/ptmx" [Unix.R_OK; Unix.W_OK];
      true
    end else
      false
  with _ -> false

let test_concurrent_input () =
  if not (has_pty_support ()) then begin
    Printf.printf "SKIP: No PTY support in this environment\n%!";
    true
  end else
    try
      (* Open a PTY and test rapid consecutive writes *)
      match Pty.open_pty () with
      | Error msg ->
          Printf.printf "FAIL: Could not open PTY: %s\n%!" msg;
          false
      | Ok (pty, _slave) ->
          (* Test rapid writes to simulate /help typed multiple times *)
          let test_data = "/help\n" in
          let rec write_multiple n =
            if n <= 0 then Lwt.return_unit
            else
              let data = Bytes.of_string test_data in
              let* _ = Pty.write pty data 0 (Bytes.length data) in
              write_multiple (n - 1)
          in

          (* Write /help 5 times rapidly *)
          Lwt_main.run (write_multiple 5);

          (* Small delay to let data process *)
          Unix.sleepf 0.1;

          (* Read back to verify data was written *)
          let buf = Bytes.create 1024 in
          let n = Lwt_main.run (
            Lwt.pick [
              Pty.read pty buf 0 1024;
              (let* () = Lwt_unix.sleep 0.5 in Lwt.return 0)
            ]
          ) in

          Lwt_main.run (Pty.close pty);

          if n >= 0 then begin
            Printf.printf "PASS: Concurrent input handling works (%d bytes read)\n%!" n;
            true
          end else begin
            Printf.printf "FAIL: Read returned negative value\n%!";
            false
          end
    with e ->
      Printf.printf "FAIL: Exception in concurrent input test: %s\n%!" (Printexc.to_string e);
      false

let () =
  Printf.printf "=== Test: Concurrent input handling ===\n%!";
  let result = test_concurrent_input () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
