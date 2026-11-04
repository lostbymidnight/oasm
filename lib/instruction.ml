let register_count = 8

type reg = int
type operand = Register of reg | Immediate of int
type alu_op = Add | Sub | Mul | Div | Mod | And | Or | Xor
type condition = Equal | Not_equal | Less | Less_equal | Greater | Greater_equal

type t =
  | Pass
  | Halt
  | Set of reg * int
  | Copy of reg * reg
  | Alu of alu_op * reg * reg * reg
  | Not of reg * reg
  | Lshift of reg * reg
  | Rshift of reg * reg
  | Inc of reg
  | Dec of reg
  | Compare of reg * operand
  | Jump of string * int
  | If of condition * int
  | Break of int
  | End_loop of int
  | Call of string * int
  | Return
  | Read of reg * reg
  | Write of reg * reg
  | Push of reg
  | Pop of reg

let alu_ops =
  [
    ("ADD", Add);
    ("SUB", Sub);
    ("MUL", Mul);
    ("DIV", Div);
    ("MOD", Mod);
    ("AND", And);
    ("OR", Or);
    ("XOR", Xor);
  ]

let conditions =
  [
    ("EQUAL", Equal);
    ("NOT_EQUAL", Not_equal);
    ("LESS", Less);
    ("LESS_EQUAL", Less_equal);
    ("GREATER", Greater);
    ("GREATER_EQUAL", Greater_equal);
  ]

let keywords =
  [
    "PASS"; "HALT"; "RETURN"; "SET"; "COPY"; "NOT"; "LSHIFT"; "RSHIFT"; "INC";
    "DEC"; "COMPARE"; "JUMP"; "CALL"; "READ"; "WRITE"; "PUSH"; "POP"; "LOOP";
    "BREAK"; "END";
  ]

let if_prefix = "IF_"

let strip_prefix ~prefix s =
  if String.starts_with ~prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let condition_of_keyword keyword =
  Option.bind (strip_prefix ~prefix:if_prefix keyword) (fun name ->
      List.assoc_opt name conditions)

let is_keyword name =
  List.mem name keywords
  || List.mem_assoc name alu_ops
  || condition_of_keyword name <> None

let name_of table value = fst (List.find (fun (_, v) -> v = value) table)
let if_keyword condition = if_prefix ^ name_of conditions condition
