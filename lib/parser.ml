open Instruction

type item = { line : int; index : int; keyword : string; args : string list }
type block = Loop_block of int * int * int list | If_block of int * int

type progress = {
  items : item list;
  labels : (string * int) list;
  count : int;
  stack : block list;
  targets : (int * int) list;
}

let max_program_size = 255
let ( let* ) = Result.bind
let fail = Error.fail

let traverse f items =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
        let* y = f x in
        go (y :: acc) rest
  in
  go [] items

let tokenize text =
  let code =
    match String.index_opt text ';' with
    | Some i -> String.sub text 0 i
    | None -> text
  in
  String.map (function ',' | '\t' | '\r' -> ' ' | c -> c) code
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")

let parse_register line token =
  let upper = String.uppercase_ascii token in
  let index =
    if String.length upper = 2 && upper.[0] = 'R' then
      int_of_string_opt (String.sub upper 1 1)
    else None
  in
  match index with
  | Some i when i >= 0 && i < register_count -> Ok i
  | _ -> fail line "unknown register '%s'" token

let parse_immediate line token =
  match int_of_string_opt token with
  | Some v when v >= -128 && v <= 255 -> Ok (v land 0xFF)
  | Some _ -> fail line "value '%s' does not fit in 8 bits" token
  | None -> fail line "invalid number '%s'" token

let parse_operand line token =
  if token <> "" && Char.uppercase_ascii token.[0] = 'R' then
    Result.map (fun r -> Register r) (parse_register line token)
  else Result.map (fun v -> Immediate v) (parse_immediate line token)

let resolve_label line labels name =
  match List.assoc_opt name labels with
  | Some index -> Ok index
  | None -> fail line "undefined label '%s'" name

let assemble ~labels ~targets { line; index; keyword; args } =
  let reg = parse_register line in
  let label = resolve_label line labels in
  let block_target () = List.assoc index targets in
  let one make a =
    let* a = reg a in
    Ok (make a)
  in
  let two make a b =
    let* a = reg a in
    let* b = reg b in
    Ok (make a b)
  in
  let three make a b c =
    let* a = reg a in
    let* b = reg b in
    let* c = reg c in
    Ok (make a b c)
  in
  match (String.uppercase_ascii keyword, args) with
  | "PASS", [] -> Ok Pass
  | "HALT", [] -> Ok Halt
  | "RETURN", [] -> Ok Return
  | "BREAK", [] -> Ok (Break (block_target ()))
  | "END", [] -> Ok (End_loop (block_target ()))
  | "SET", [ d; v ] ->
      let* d = reg d in
      let* v = parse_immediate line v in
      Ok (Set (d, v))
  | "COPY", [ d; s ] -> two (fun d s -> Copy (d, s)) d s
  | m, [ d; a; b ] when List.mem_assoc m alu_ops ->
      three (fun d a b -> Alu (List.assoc m alu_ops, d, a, b)) d a b
  | "NOT", [ d; s ] -> two (fun d s -> Not (d, s)) d s
  | "LSHIFT", [ d; s ] -> two (fun d s -> Lshift (d, s)) d s
  | "RSHIFT", [ d; s ] -> two (fun d s -> Rshift (d, s)) d s
  | "INC", [ d ] -> one (fun d -> Inc d) d
  | "DEC", [ d ] -> one (fun d -> Dec d) d
  | "COMPARE", [ a; b ] ->
      let* a = reg a in
      let* b = parse_operand line b in
      Ok (Compare (a, b))
  | "JUMP", [ name ] ->
      let* target = label name in
      Ok (Jump (name, target))
  | m, [] when Option.is_some (condition_of_keyword m) ->
      Ok (If (Option.get (condition_of_keyword m), block_target ()))
  | "CALL", [ name ] ->
      let* target = label name in
      Ok (Call (name, target))
  | "READ", [ d; a ] -> two (fun d a -> Read (d, a)) d a
  | "WRITE", [ a; s ] -> two (fun a s -> Write (a, s)) a s
  | "PUSH", [ s ] -> one (fun s -> Push s) s
  | "POP", [ d ] -> one (fun d -> Pop d) d
  | m, _ when is_keyword m -> fail line "wrong operands for %s" m
  | _ -> fail line "unknown instruction '%s'" keyword

let emit progress line keyword args =
  {
    progress with
    items =
      { line; index = progress.count; keyword; args } :: progress.items;
    count = progress.count + 1;
  }

let rec add_break index = function
  | [] -> None
  | Loop_block (line, start, breaks) :: rest ->
      Some (Loop_block (line, start, index :: breaks) :: rest)
  | block :: rest ->
      Option.map (fun rest -> block :: rest) (add_break index rest)

let statement progress line keyword args =
  match (String.uppercase_ascii keyword, args) with
  | "LOOP", [] ->
      Ok
        {
          progress with
          stack = Loop_block (line, progress.count, []) :: progress.stack;
        }
  | "BREAK", [] -> (
      match add_break progress.count progress.stack with
      | Some stack -> Ok (emit { progress with stack } line "BREAK" [])
      | None -> fail line "BREAK outside of a LOOP")
  | "END", [] -> (
      match progress.stack with
      | [] -> fail line "END without a matching LOOP or IF"
      | If_block (_, index) :: stack ->
          Ok
            {
              progress with
              stack;
              targets = (index, progress.count) :: progress.targets;
            }
      | Loop_block (_, start, breaks) :: stack ->
          let progress = emit { progress with stack } line "END" [] in
          let exit_index = progress.count in
          let jumps =
            (exit_index - 1, start) :: List.map (fun b -> (b, exit_index)) breaks
          in
          Ok { progress with targets = jumps @ progress.targets })
  | (("LOOP" | "BREAK" | "END") as keyword), _ :: _ ->
      fail line "wrong operands for %s" keyword
  | m, [] when Option.is_some (condition_of_keyword m) ->
      let progress = emit progress line m [] in
      Ok
        {
          progress with
          stack = If_block (line, progress.count - 1) :: progress.stack;
        }
  | _ -> Ok (emit progress line keyword args)

(* jmp uses labels *)
let collect lines =
  let rec go progress = function
    | [] -> (
        match progress.stack with
        | [] -> Ok progress
        | Loop_block (line, _, _) :: _ -> fail line "LOOP is never closed"
        | If_block (line, _) :: _ -> fail line "IF is never closed")
    | (_, []) :: rest -> go progress rest
    | (line, first :: operands) :: rest
      when String.ends_with ~suffix:":" first -> (
        let name = String.sub first 0 (String.length first - 1) in
        if name = "" then fail line "empty label"
        else if List.mem_assoc name progress.labels then
          fail line "duplicate label '%s'" name
        else
          let progress =
            { progress with labels = (name, progress.count) :: progress.labels }
          in
          match operands with
          | [] -> go progress rest
          | keyword :: args ->
              let* progress = statement progress line keyword args in
              go progress rest)
    | (line, keyword :: args) :: rest ->
        let* progress = statement progress line keyword args in
        go progress rest
  in
  go { items = []; labels = []; count = 0; stack = []; targets = [] } lines

let parse source =
  let lines =
    String.split_on_char '\n' source
    |> List.mapi (fun i text -> (i + 1, tokenize text))
  in
  let* { items; labels; count; targets; _ } = collect lines in
  if count > max_program_size then
    Error
      (Printf.sprintf "program has %d instructions, the maximum is %d" count
         max_program_size)
  else
    (* have to process here for jmp :c *)
    let* program = traverse (assemble ~labels ~targets) (List.rev items) in
    Ok (Array.of_list program)
