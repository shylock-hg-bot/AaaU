(** Test shell-style parsing of client --program values. *)

let fail fmt = Printf.ksprintf failwith fmt

let expect_split input expected_program expected_args =
  match AaaU.Command_line.split_command input with
  | Error e -> fail "expected success for %S, got error: %s" input e
  | Ok (program, args) ->
    if program <> expected_program then
      fail "expected program %S for %S, got %S" expected_program input program;
    if args <> expected_args then
      fail "expected args [%s] for %S, got [%s]"
        (String.concat "; " expected_args)
        input
        (String.concat "; " args)

let expect_error input =
  match AaaU.Command_line.split_command input with
  | Ok (program, args) ->
    fail "expected parse error for %S, got %S with [%s]"
      input program (String.concat "; " args)
  | Error _ -> ()

let () =
  Printf.printf "=== Test: Command line split ===\n%!";
  expect_split "codex --dangerously-bypass-approvals-and-sandbox"
    "codex" ["--dangerously-bypass-approvals-and-sandbox"];
  expect_split "codex --model 'gpt 5.4'" "codex" ["--model"; "gpt 5.4"];
  expect_split "  /bin/echo \"hello world\"  " "/bin/echo" ["hello world"];
  expect_error "codex 'unterminated";
  Printf.printf "PASS\n%!"
