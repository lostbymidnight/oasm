let memory_size = 256
let stack_top = 255

type flags = {
  mutable zero : bool;
  mutable carry : bool;
  mutable negative : bool;
}

type t = {
  program : Instruction.t array;
  registers : int array;
  memory : Bytes.t;
  flags : flags;
  mutable pc : int;
  mutable sp : int;
}

let create program =
  {
    program;
    registers = Array.make Instruction.register_count 0;
    memory = Bytes.make memory_size '\000';
    flags = { zero = false; carry = false; negative = false };
    pc = 0;
    sp = stack_top;
  }

let update_flags m ~carry result =
  m.flags.zero <- result = 0;
  m.flags.carry <- carry;
  m.flags.negative <- result land 0x80 <> 0

let push m value =
  if m.sp < 0 then raise (Error.Fault "stack overflow");
  Bytes.set_uint8 m.memory m.sp value;
  m.sp <- m.sp - 1

let pop m =
  if m.sp >= stack_top then raise (Error.Fault "stack underflow");
  m.sp <- m.sp + 1;
  Bytes.get_uint8 m.memory m.sp
