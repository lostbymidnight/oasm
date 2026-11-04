module Error = Error
module Instruction = Instruction
module Parser = Parser
module State = State
module Trace = Trace

open Instruction

type status = Running | Halted
type outcome = { status : status; trace : string list }

let ( let* ) = Result.bind
let parse = Parser.parse
let create = State.create

let alu_compute op a b =
  match op with
  | Add -> ((a + b) land 0xFF, a + b > 0xFF)
  | Sub -> ((a - b) land 0xFF, a < b)
  | Mul -> (a * b land 0xFF, a * b > 0xFF)
  | Div ->
      if b = 0 then raise (Error.Fault "division by zero") else (a / b, false)
  | Mod ->
      if b = 0 then raise (Error.Fault "division by zero") else (a mod b, false)
  | And -> (a land b, false)
  | Or -> (a lor b, false)
  | Xor -> (a lxor b, false)

let holds (m : State.t) = function
  | Equal -> m.flags.zero
  | Not_equal -> not m.flags.zero
  | Less -> m.flags.carry
  | Less_equal -> m.flags.carry || m.flags.zero
  | Greater -> (not m.flags.carry) && not m.flags.zero
  | Greater_equal -> not m.flags.carry

let execute (m : State.t) instruction =
  let header = Trace.header m and note = Trace.note in
  let get r = m.registers.(r) in
  let set r value = m.registers.(r) <- value in
  match instruction with
  | Pass ->
      (* no-op *)
      (Running, [ header "PASS" ])
  | Set (d, value) ->
      (* immediate load *)
      let before = note d (get d) in
      set d value;
      (Running, [ header "SET"; before; Printf.sprintf " -> %d" value ])
  | Copy (d, s) ->
      let before = note d (get d) in
      set d (get s);
      (Running, [ header "COPY"; before; note s (get s) ])
  | Alu (op, d, a, b) ->
      let a_val = get a and b_val = get b in
      let result, carry = alu_compute op a_val b_val in
      set d result;
      State.update_flags m ~carry result;
      ( Running,
        [
          header (name_of alu_ops op);
          Printf.sprintf " -> R%d (%d %s %d = %d)" d a_val
            (Trace.alu_symbol op) b_val result;
          note a a_val;
          note b b_val;
        ] )
  | Not (d, s) ->
      let value = get s in
      let result = lnot value land 0xFF in
      set d result;
      State.update_flags m ~carry:false result;
      ( Running,
        [
          header "NOT";
          Printf.sprintf " -> R%d (~%d = %d)" d value result;
          note s value;
        ] )
  | Lshift (d, s) ->
      let value = get s in
      let result = (value lsl 1) land 0xFF in
      set d result;
      State.update_flags m ~carry:(value land 0x80 <> 0) result;
      ( Running,
        [
          header "LSHIFT";
          Printf.sprintf " -> R%d (%d << 1 = %d)" d value result;
          note s value;
        ] )
  | Rshift (d, s) ->
      let value = get s in
      let result = value lsr 1 in
      set d result;
      State.update_flags m ~carry:(value land 1 <> 0) result;
      ( Running,
        [
          header "RSHIFT";
          Printf.sprintf " -> R%d (%d >> 1 = %d)" d value result;
          note s value;
        ] )
  | Inc r ->
      let value = get r in
      let result = (value + 1) land 0xFF in
      set r result;
      State.update_flags m ~carry:m.flags.carry result;
      ( Running,
        [ header "INC"; Printf.sprintf " -> R%d (%d + 1 = %d)" r value result ]
      )
  | Dec r ->
      let value = get r in
      let result = (value - 1) land 0xFF in
      set r result;
      State.update_flags m ~carry:m.flags.carry result;
      ( Running,
        [ header "DEC"; Printf.sprintf " -> R%d (%d - 1 = %d)" r value result ]
      )
  | Compare (a, operand) ->
      let a_val = get a in
      let b_val, shown =
        match operand with
        | Register b ->
            (get b, Printf.sprintf "R%d (%d)" b (get b))
        | Immediate v -> (v, string_of_int v)
      in
      State.update_flags m ~carry:(a_val < b_val) ((a_val - b_val) land 0xFF);
      ( Running,
        [
          header "COMPARE";
          Printf.sprintf " -> R%d (%d) vs %s" a a_val shown;
          Trace.flags_line m;
        ] )
  | Jump (label, target) ->
      let head = header "JUMP" in
      m.pc <- target;
      (Running, [ head; Printf.sprintf " -> %s (%d)" label target ])
  | If (condition, target) ->
      let head = header (if_keyword condition) in
      if holds m condition then (Running, [ head; " -> true" ])
      else (
        m.pc <- target;
        (Running, [ head; Printf.sprintf " -> false (%d)" target ]))
  | Break target ->
      let head = header "BREAK" in
      m.pc <- target;
      (Running, [ head; Printf.sprintf " -> %d" target ])
  | End_loop target ->
      let head = header "END" in
      m.pc <- target;
      (Running, [ head; Printf.sprintf " -> loop (%d)" target ])
  | Call (label, target) ->
      let head = header "CALL" in
      State.push m m.pc;
      m.pc <- target;
      ( Running,
        [
          head;
          Printf.sprintf " -> %s (%d)" label target;
          Printf.sprintf " -> SP (%d)" m.sp;
        ] )
  | Return ->
      let head = header "RETURN" in
      let address = State.pop m in
      m.pc <- address;
      ( Running,
        [
          head;
          Printf.sprintf " -> PC (%d)" address;
          Printf.sprintf " -> SP (%d)" m.sp;
        ] )
  | Read (d, a) ->
      let address = get a in
      let value = Bytes.get_uint8 m.memory address in
      let before = note d (get d) in
      set d value;
      ( Running,
        [ header "READ"; before; Printf.sprintf " -> [%d] = %d" address value ] )
  | Write (a, s) ->
      let address = get a and value = get s in
      let old = Bytes.get_uint8 m.memory address in
      Bytes.set_uint8 m.memory address value;
      ( Running,
        [
          header "WRITE";
          Printf.sprintf " -> [%d] (%d -> %d)" address old value;
          note s value;
        ] )
  | Push s ->
      let value = get s in
      State.push m value;
      ( Running,
        [ header "PUSH"; note s value; Printf.sprintf " -> SP (%d)" m.sp ] )
  | Pop d ->
      let before = note d (get d) in
      set d (State.pop m);
      (Running, [ header "POP"; before; Printf.sprintf " -> SP (%d)" m.sp ])
  | Halt ->
      (* instantaneous death *)
      (Halted, header "HALT" :: "" :: Trace.register_dump m)

let step (m : State.t) =
  if m.pc >= Array.length m.program then
    Ok { status = Halted; trace = Trace.register_dump m }
  else
    let instruction = m.program.(m.pc) in
    (* equivalent of pc := 1 *)
    m.pc <- m.pc + 1;
    match execute m instruction with
    | status, trace -> Ok { status; trace }
    | exception Error.Fault message ->
        Error (Printf.sprintf "%s (instruction %d)" message m.pc)

let run ?(on_step = ignore) m =
  let rec loop () =
    let* outcome = step m in
    on_step outcome.trace;
    match outcome.status with Running -> loop () | Halted -> Ok ()
  in
  loop ()
