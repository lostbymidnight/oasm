let failures = ref 0

let check name condition =
  if not condition then (
    incr failures;
    Printf.printf "FAIL: %s\n" name)

let run_source source =
  match Oasm.parse source with
  | Error message -> failwith ("parse failed: " ^ message)
  | Ok program -> (
      let machine = Oasm.create program in
      match Oasm.run machine with
      | Ok () -> machine
      | Error message -> failwith ("run failed: " ^ message))

let error_of source =
  match Oasm.parse source with
  | Error message -> Some message
  | Ok program -> (
      match Oasm.run (Oasm.create program) with
      | Error message -> Some message
      | Ok () -> None)

let contains ~sub s =
  let n = String.length sub and len = String.length s in
  let rec go i = i + n <= len && (String.sub s i n = sub || go (i + 1)) in
  go 0

let check_error name source expected =
  match error_of source with
  | Some message -> check name (contains ~sub:expected message)
  | None -> check name false

let reg (m : Oasm.State.t) i = m.registers.(i)

let core_tests () =
  let m = run_source "SET R0, 255\nSET R1, 1\nADD R2, R0, R1\nHALT" in
  check "add wraps to 0" (reg m 2 = 0);
  check "add sets carry" m.flags.carry;
  check "add sets zero" m.flags.zero;

  let m = run_source "SET R0, 0\nSET R1, 1\nSUB R2, R0, R1\nHALT" in
  check "sub wraps to 255" (reg m 2 = 255);
  check "sub sets borrow" m.flags.carry;
  check "sub sets negative" m.flags.negative;

  let m =
    run_source
      "SET R0, 17\nSET R1, 5\nMUL R2, R0, R1\nDIV R3, R0, R1\nMOD R4, R0, R1\nHALT"
  in
  check "mul" (reg m 2 = 85);
  check "div" (reg m 3 = 3);
  check "mod" (reg m 4 = 2);

  let m = run_source "SET R0, 16\nSET R1, 20\nMUL R2, R0, R1\nHALT" in
  check "mul wraps" (reg m 2 = 64);
  check "mul sets carry" m.flags.carry;

  let m =
    run_source
      "SET R0, 12\nSET R1, 10\nAND R2, R0, R1\nOR R3, R0, R1\nXOR R4, R0, R1\nNOT R5, R0\nHALT"
  in
  check "and" (reg m 2 = 8);
  check "or" (reg m 3 = 14);
  check "xor" (reg m 4 = 6);
  check "not" (reg m 5 = 243);

  let m = run_source "SET R0, 129\nLSHIFT R1, R0\nRSHIFT R2, R0\nHALT" in
  check "shl" (reg m 1 = 2);
  check "shr" (reg m 2 = 64);
  check "shr carry" m.flags.carry;

  let m = run_source "SET R0, 255\nINC R0\nSET R1, 0\nDEC R1\nHALT" in
  check "inc wraps" (reg m 0 = 0);
  check "dec wraps" (reg m 1 = 255);

  let m = run_source "SET R0, -1\nCOPY R1, R0\nHALT" in
  check "negative immediate" (reg m 0 = 255);
  check "mov" (reg m 1 = 255);

  let m =
    run_source
      "SET R0, 5\nSET R1, 0\nLOOP\nADD R1, R1, R0\nDEC R0\nCOMPARE R0, 0\nIF_EQUAL\nBREAK\nEND\nEND\nHALT"
  in
  check "loop sums 5..1" (reg m 1 = 15);

  let m =
    run_source
      "SET R0, 3\nCOMPARE R0, 5\nIF_LESS\nSET R1, 1\nEND\nIF_GREATER_EQUAL\nSET R2, 1\nEND\nHALT"
  in
  check "if taken" (reg m 1 = 1);
  check "if skipped" (reg m 2 = 0);

  let compare_result a b condition =
    let m =
      run_source
        (Printf.sprintf "SET R0, %d\nSET R1, %d\nCOMPARE R0, R1\nIF_%s\nSET R2, 1\nEND\nHALT"
           a b condition)
    in
    reg m 2 = 1
  in
  check "equal" (compare_result 4 4 "EQUAL");
  check "not equal" (compare_result 4 5 "NOT_EQUAL");
  check "less" (compare_result 4 5 "LESS");
  check "less equal" (compare_result 5 5 "LESS_EQUAL");
  check "greater" (compare_result 6 5 "GREATER");
  check "greater equal" (compare_result 5 5 "GREATER_EQUAL");
  check "not less" (not (compare_result 5 5 "LESS"));
  check "not greater" (not (compare_result 5 5 "GREATER"));

  let m =
    run_source
      "SET R0, 0\nSET R1, 0\nLOOP\nINC R0\nSET R2, 0\nLOOP\nINC R2\nINC R1\nCOMPARE R2, 3\nIF_EQUAL\nBREAK\nEND\nEND\nCOMPARE R0, 2\nIF_EQUAL\nBREAK\nEND\nEND\nHALT"
  in
  check "nested loops" (reg m 1 = 6);
  check "break leaves only the inner loop" (reg m 0 = 2);

  let m = run_source "SET R0, 1\nJUMP skip\nSET R0, 2\nskip:\nHALT" in
  check "jump" (reg m 0 = 1);

  let m = run_source "SET R0, 7\nCOMPARE R0, R0\nIF_EQUAL\nSET R1, 9\nEND" in
  check "if at end of program" (reg m 1 = 9);

  let m =
    run_source "SET R0, 200\nSET R1, 7\nWRITE R0, R1\nREAD R2, R0\nHALT"
  in
  check "memory store" (Bytes.get_uint8 m.memory 200 = 7);
  check "memory load" (reg m 2 = 7);

  let m = run_source "SET R0, 9\nPUSH R0\nSET R0, 0\nPOP R1\nHALT" in
  check "push and pop" (reg m 1 = 9);
  check "stack pointer restored" (m.sp = Oasm.State.stack_top);

  let m = run_source "SET R0, 1\nCALL double\nHALT\ndouble:\nADD R0, R0, R0\nRETURN" in
  check "call and ret" (reg m 0 = 2);

  let m = run_source "SET R0, 1" in
  check "falling off the end halts" (reg m 0 = 1);

  let m = run_source "start: SET R0, 4 ; trailing comment\nHALT" in
  check "label on same line and inline comment" (reg m 0 = 4);

  check_error "unknown instruction" "FOO R0" "unknown instruction 'FOO'";
  check_error "bad register" "SET R9, 1" "unknown register 'R9'";
  check_error "wrong operand count" "SET R0" "wrong operands for SET";
  check_error "break outside loop" "BREAK" "BREAK outside of a LOOP";
  check_error "end without block" "END" "END without a matching";
  check_error "unclosed loop" "LOOP\nPASS" "LOOP is never closed";
  check_error "unclosed if" "COMPARE R0, 0\nIF_EQUAL\nPASS" "IF is never closed";
  check_error "if takes no operands" "IF_EQUAL R0" "wrong operands for IF_EQUAL";
  check_error "undefined label" "JUMP nowhere" "undefined label 'nowhere'";
  check_error "duplicate label" "a:\na:\nHALT" "duplicate label 'a'";
  check_error "immediate too large" "SET R0, 256" "does not fit in 8 bits";
  check_error "division by zero"
    "SET R0, 1\nSET R1, 0\nDIV R2, R0, R1"
    "division by zero";
  check_error "stack underflow" "POP R0" "stack underflow";
  check_error "stack overflow" "loop:\nPUSH R0\nJUMP loop" "stack overflow";
  check_error "program too large"
    (String.concat "\n" (List.init 256 (fun _ -> "PASS")))
    "maximum is 255"

let memory (m : Oasm.State.t) address = Bytes.get_uint8 m.memory address
let read_program name = In_channel.with_open_text ("programs/" ^ name) In_channel.input_all
let run_program name = run_source (read_program name)

let trace_of source steps =
  match Oasm.parse source with
  | Error message -> [ "parse error: " ^ message ]
  | Ok program ->
      let machine = Oasm.create program in
      let rec go n last =
        if n = 0 then last
        else
          match Oasm.step machine with
          | Ok outcome -> go (n - 1) outcome.Oasm.trace
          | Error message -> [ "run error: " ^ message ]
      in
      go steps []

let feature_tests () =
  let m = run_source "SET R0, 5\nPASS\nHALT\nSET R0, 9" in
  check "pass changes nothing and halt stops execution" (reg m 0 = 5);

  let m = run_source "set r0, 0xFF\nset r1, 0b101\nCopy R2, r1\nhalt" in
  check "hex literal" (reg m 0 = 255);
  check "binary literal" (reg m 1 = 5);
  check "keywords and registers are case-insensitive" (reg m 2 = 5);

  let m = run_source "SET R0, -128\nHALT" in
  check "negative immediate wraps to a byte" (reg m 0 = 128);

  let m = run_source "SET R0, 5\nCOMPARE R0, 5\nHALT" in
  check "compare equal sets zero" (m.flags.zero && not m.flags.carry);
  check "compare leaves registers alone" (reg m 0 = 5);

  let m = run_source "SET R0, 3\nCOMPARE R0, 5\nHALT" in
  check "compare less sets carry" (m.flags.carry && not m.flags.zero);
  check "compare less sets negative" m.flags.negative;

  let m = run_source "SET R0, 5\nSET R1, 3\nCOMPARE R0, R1\nHALT" in
  check "compare against a register" (not m.flags.carry && not m.flags.zero);

  let m = run_source "SET R0, 128\nLSHIFT R1, R0\nHALT" in
  check "lshift carries out the top bit" (reg m 1 = 0 && m.flags.carry && m.flags.zero);

  let m = run_source "SET R0, 1\nRSHIFT R1, R0\nHALT" in
  check "rshift carries out the bottom bit" (reg m 1 = 0 && m.flags.carry && m.flags.zero);

  let m = run_source "SET R0, 255\nSET R1, 1\nADD R2, R0, R1\nINC R3\nHALT" in
  check "inc keeps the carry flag" m.flags.carry;

  let m = run_source "SET R0, 16\nSET R1, 16\nMUL R2, R0, R1\nHALT" in
  check "16 * 16 wraps to zero" (reg m 2 = 0 && m.flags.carry && m.flags.zero);

  let m =
    run_source
      "SET R0, 1\nSET R1, 6\nLOOP\nMUL R0, R0, R1\nDEC R1\nCOMPARE R1, 0\nIF_EQUAL\nBREAK\nEND\nEND\nHALT"
  in
  check "6! overflows a byte" (reg m 0 = 720 land 0xFF);

  let m = run_source "SET R0, 255\nCOMPARE R0, 0\nIF_GREATER\nSET R1, 1\nEND\nHALT" in
  check "comparisons are unsigned" (reg m 1 = 1);

  let m =
    run_source
      "SET R0, 0\nSET R1, 0\nWRITE R0, R1\nSET R0, 255\nSET R1, 22\nWRITE R0, R1\nHALT"
  in
  check "write to the lowest address" (memory m 0 = 0);
  check "write to the highest address" (memory m 255 = 22);

  let m = run_source "SET R0, 100\nSET R1, 9\nREAD R1, R0\nHALT" in
  check "memory starts zeroed" (reg m 1 = 0);

  let m =
    run_source
      "SET R0, 1\nPUSH R0\nSET R0, 2\nPUSH R0\nSET R0, 3\nPUSH R0\nHALT"
  in
  check "three pushes move the stack pointer" (m.sp = Oasm.State.stack_top - 3);

  let m =
    run_source
      "SET R0, 1\nPUSH R0\nSET R0, 2\nPUSH R0\nSET R0, 3\nPUSH R0\nPOP R1\nPOP R2\nPOP R3\nHALT"
  in
  check "stack is last in first out" (reg m 1 = 3 && reg m 2 = 2 && reg m 3 = 1);

  let m =
    run_source
      "SET R0, 1\nCALL outer\nHALT\nouter:\nADD R0, R0, R0\nCALL inner\nADD R0, R0, R0\nRETURN\ninner:\nINC R0\nRETURN"
  in
  check "nested calls" (reg m 0 = 6);
  check "calls leave the stack empty" (m.sp = Oasm.State.stack_top);

  let m =
    run_source
      "SET R0, 5\nCOMPARE R0, 5\nIF_EQUAL\nCOMPARE R0, 9\nIF_LESS\nSET R1, 1\nEND\nSET R2, 1\nEND\nHALT"
  in
  check "nested ifs both taken" (reg m 1 = 1 && reg m 2 = 1);

  let m =
    run_source
      "SET R0, 5\nCOMPARE R0, 5\nIF_EQUAL\nCOMPARE R0, 1\nIF_LESS\nSET R1, 1\nEND\nSET R2, 1\nEND\nHALT"
  in
  check "inner if false only skips the inner body" (reg m 1 = 0 && reg m 2 = 1);

  let m =
    run_source
      "SET R0, 5\nCOMPARE R0, 6\nIF_EQUAL\nCOMPARE R0, 9\nIF_LESS\nSET R1, 1\nEND\nSET R2, 1\nEND\nHALT"
  in
  check "outer if false skips everything inside" (reg m 1 = 0 && reg m 2 = 0);

  let m = run_source "COMPARE R0, 0\nIF_EQUAL\nEND\nSET R1, 3\nHALT" in
  check "empty if body" (reg m 1 = 3);

  let m = run_source "LOOP\nBREAK\nEND\nSET R0, 1" in
  check "break on the first pass" (reg m 0 = 1);

  let m =
    run_source
      "SET R0, 0\nLOOP\nINC R0\nCOMPARE R0, 3\nIF_EQUAL\nCOMPARE R0, 3\nIF_EQUAL\nBREAK\nEND\nEND\nEND\nHALT"
  in
  check "break from inside nested ifs" (reg m 0 = 3);

  let m = run_source "SET R0, 0\nagain: LOOP\nINC R0\nBREAK\nEND\nHALT" in
  check "label on the same line as loop" (reg m 0 = 1);

  let m = run_source "SET R0, 0\nLOOP\nINC R0\nJUMP out\nEND\nout:\nHALT" in
  check "jump out of a loop" (reg m 0 = 1);

  let m =
    run_source
      "SET R0, 0\nLOOP\nINC R0\nCOMPARE R0, 200\nIF_EQUAL\nBREAK\nEND\nEND\nHALT"
  in
  check "loop runs many times" (reg m 0 = 200);

  check "set trace"
    (trace_of "SET R0, 42" 1 = [ "SET (1)"; " -> R0 (0)"; " -> 42" ]);
  check "compare immediate trace"
    (trace_of "SET R0, 1\nCOMPARE R0, 1" 2
    = [ "COMPARE (2)"; " -> R0 (1) vs 1"; " -> flags (Z=1 C=0 N=0)" ]);
  check "if true trace"
    (trace_of "SET R0, 1\nCOMPARE R0, 1\nIF_EQUAL\nPASS\nEND" 3
    = [ "IF_EQUAL (3)"; " -> true" ]);
  check "if false trace"
    (trace_of "SET R0, 2\nCOMPARE R0, 1\nIF_EQUAL\nPASS\nEND" 3
    = [ "IF_EQUAL (3)"; " -> false (4)" ]);
  check "break trace"
    (trace_of "LOOP\nBREAK\nEND\nHALT" 1 = [ "BREAK (1)"; " -> 2" ]);
  check "end of loop trace"
    (trace_of "LOOP\nPASS\nEND" 2 = [ "END (2)"; " -> loop (0)" ]);

  let steps = ref 0 in
  (match Oasm.parse "SET R0, 1\nHALT" with
  | Ok program ->
      ignore (Oasm.run ~on_step:(fun _ -> incr steps) (Oasm.create program))
  | Error _ -> ());
  check "run reports every step including halt" (!steps = 2);

  check_error "loop takes no operands" "LOOP R0\nEND" "wrong operands for LOOP";
  check_error "end takes no operands" "END R0" "wrong operands for END";
  check_error "break takes no operands" "BREAK R0" "wrong operands for BREAK";
  check_error "block errors name the line" "PASS\nBREAK" "line 2:";
  check_error "compare with a bad register" "COMPARE R0, R9" "unknown register 'R9'";
  check_error "compare with a huge number" "COMPARE R0, 300" "does not fit in 8 bits";
  check_error "runtime errors name the instruction"
    "SET R0, 0\nDIV R1, R0, R0" "instruction 2";
  check_error "old names are gone" "LOAD R0, 1" "unknown instruction 'LOAD'"

let program_tests () =
  let m = run_program "fibonacci.oasm" in
  check "fibonacci: tenth number" (reg m 0 = 55);
  check "fibonacci: eleventh number" (reg m 1 = 89);

  let m = run_program "factorial.oasm" in
  check "factorial: 5!" (reg m 0 = 120);

  let m = run_program "gcd.oasm" in
  check "gcd: gcd(48, 18)" (reg m 0 = 6);

  let m = run_program "collatz.oasm" in
  check "collatz: 7 takes 16 steps" (reg m 1 = 16);
  check "collatz: ends at 1" (reg m 0 = 1);

  let m = run_program "bubble_sort.oasm" in
  check "bubble sort: memory is sorted"
    (List.init 5 (memory m) = [ 1; 2; 5; 7; 9 ]);

  let m = run_program "primes.oasm" in
  check "primes: ten primes below 30" (reg m 6 = 10);
  check "primes: 4 is marked composite" (memory m 4 = 1);
  check "primes: 7 is left unmarked" (memory m 7 = 0);
  check "primes: 29 is left unmarked" (memory m 29 = 0);
  check "primes: 27 is marked composite" (memory m 27 = 1);

  let m = run_program "stack_reverse.oasm" in
  check "stack reverse: order is reversed"
    (List.init 5 (memory m) = [ 5; 4; 3; 2; 1 ]);
  check "stack reverse: stack is empty again" (m.sp = Oasm.State.stack_top);

  let m = run_program "recursion.oasm" in
  check "recursion: sum(5)" (reg m 1 = 15);
  check "recursion: stack is empty again" (m.sp = Oasm.State.stack_top);

  let m = run_program "bit_width.oasm" in
  check "bit width: a byte has eight bits" (reg m 1 = 8)

let () =
  core_tests ();
  feature_tests ();
  program_tests ();
  if !failures > 0 then exit 1
