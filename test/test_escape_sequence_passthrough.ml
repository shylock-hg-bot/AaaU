(** Test escape sequence passthrough *)

open AaaU

let test_escape_sequence_passthrough () =
  match Pty.open_pty () with
    | Error msg ->
        Printf.printf "FAIL: Could not open PTY: %s\n%!" msg;
        false
    | Ok (pty, slave) ->
        (* Fork a child that writes escape sequences *)
        let test_data = "\x1b[?2004h\x1b[?1004hHello\n" in
        let pid = Unix.fork () in
        if pid = 0 then begin
          (* Child: open slave and write test data *)
          let fd = Unix.openfile (slave :> string) [Unix.O_RDWR] 0 in
          ignore (Unix.write_substring fd test_data 0 (String.length test_data));
          Unix.close fd;
          Unix._exit 0
        end else begin
          (* Parent: read from PTY master *)
          Unix.sleepf 0.2;
          let buf = Bytes.create 1024 in
          let n = Lwt_main.run (Pty.read pty buf 0 1024) in
          let data = Bytes.sub_string buf 0 n in

          Printf.printf "Read %d bytes, expected %d\n%!" n (String.length test_data);

          (* Check if data matches (allow for some timing variance) *)
          let result =
            if String.contains data '\x1b' then begin
              if String.starts_with ~prefix:"\x1b[?2004h" data then
                true
              else begin
                Printf.printf "FAIL: Data doesn't start with expected sequence: %S\n%!" data;
                false
              end
            end else begin
              Printf.printf "FAIL: Escape character (\\x1b) is MISSING in: %S\n%!" data;
              false
            end
          in

          ignore (Unix.waitpid [] pid);
          Lwt_main.run (Pty.close pty);
          result
        end

let () =
  Printf.printf "=== Test: Escape sequence passthrough ===\n%!";
  let result = test_escape_sequence_passthrough () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
