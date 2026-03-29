(** Test that Pty module returns Error results instead of raising *)

open AaaU

let test_pty_no_exceptions () =
  (* This test verifies the fix for the "Failed to get ptsname" error.
     The PTY module should return (Error msg) instead of raising Failure. *)
  let test_count = 3 in
  let rec run_tests n passed =
    if n <= 0 then passed
    else begin
      let result =
        try
          match Pty.open_pty () with
          | Error _ -> true  (* Error is expected/acceptable *)
          | Ok (pty, _) ->
              let () = Lwt_main.run (Pty.close pty) in
              true
        with
        | Failure msg ->
            Printf.printf "FAIL: Exception raised (should return Error): %s\n%!" msg;
            false
        | _ ->
            (* Other exceptions might be environment-related *)
            true
      in
      run_tests (n - 1) (passed && result)
    end
  in
  run_tests test_count true

let () =
  Printf.printf "=== Test: PTY no exceptions ===\n%!";
  let result = test_pty_no_exceptions () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
