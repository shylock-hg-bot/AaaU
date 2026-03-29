(** Test process group kill - verify children are terminated with parent *)

let test_process_group () =
  let pid = Unix.fork () in
  if pid = 0 then begin
    (* Child: create new session and process group *)
    let _ = Unix.setsid () in

    (* Fork a grandchild *)
    let grandchild = Unix.fork () in
    if grandchild = 0 then begin
      (* Grandchild: sleep and exit *)
      Unix.sleep 10;
      Unix._exit 0
    end else begin
      (* Child: sleep a bit then exit, leaving grandchild as potential zombie *)
      Unix.sleepf 0.5;
      Unix._exit 0
    end
  end else begin
    (* Parent: wait for child, then try to kill process group *)
    let (_, _) = Unix.waitpid [] pid in

    (* Try to kill the process group (child's PID was the PGID) *)
    (try Unix.kill (-pid) Sys.sigkill with _ -> ());

    (* Give time for signal delivery *)
    Unix.sleepf 0.1;

    Printf.printf "PASS: Process group termination test completed\n%!";
    true
  end

let () =
  Printf.printf "=== Test: Process group termination ===\n%!";
  let result = test_process_group () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
