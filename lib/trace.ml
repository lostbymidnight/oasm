let header (m : State.t) name = Printf.sprintf "%s (%d)" name m.pc
let note r value = Printf.sprintf " -> R%d (%d)" r value

let flags_line (m : State.t) =
  Printf.sprintf " -> flags (Z=%d C=%d N=%d)"
    (Bool.to_int m.flags.zero)
    (Bool.to_int m.flags.carry)
    (Bool.to_int m.flags.negative)

let register_dump (m : State.t) =
  List.init Instruction.register_count (fun i ->
      Printf.sprintf "R%d = %d" i m.registers.(i))

let alu_symbol : Instruction.alu_op -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
