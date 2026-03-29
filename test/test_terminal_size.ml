(** Test terminal size functions *)

open AaaU

let test_terminal_size () =
  try
      (* Test get_terminal_size - should return a reasonable size or fallback *)
      let (rows, cols) = Pty.get_terminal_size Unix.stdout in
      Printf.printf "Terminal size: %d rows x %d cols\n%!" rows cols;

      (* Validate reasonable terminal size *)
      let valid_size = rows > 0 && cols > 0 && rows < 1000 && cols < 1000 in
      if not valid_size then
        Printf.printf "FAIL: Invalid terminal size: %dx%d\n%!" rows cols;

      valid_size
    with e ->
      Printf.printf "FAIL: Exception getting terminal size: %s\n%!" (Printexc.to_string e);
      false

let () =
  Printf.printf "=== Test: Terminal size (get_terminal_size) ===\n%!";
  let result = test_terminal_size () in
  if result then
    Printf.printf "PASS\n%!"
  else begin
    Printf.printf "FAIL\n%!";
    exit 1
  end
