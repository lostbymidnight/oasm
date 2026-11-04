let die message =
  prerr_endline ("Error: " ^ message);
  exit 1

let read_file filename =
  try In_channel.with_open_text filename In_channel.input_all
  with Sys_error reason ->
    die (Printf.sprintf "Could not open file '%s'.\nReason: %s" filename reason)

let () =
  let filename =
    match Sys.argv with
    | [| _; filename |] -> filename
    | _ ->
        prerr_endline "Error: No filename given.";
        prerr_endline "Usage: dune exec oasm -- <filename.oasm>";
        exit 1
  in
  let program =
    match Oasm.parse (read_file filename) with
    | Ok program -> program
    | Error message -> die message
  in
  let machine = Oasm.create program in
  let start = Sys.time () in
  (* pretty formatting *)
  let print_step trace =
    print_endline "";
    List.iter print_endline trace
  in
  (match Oasm.run ~on_step:print_step machine with
  | Ok () -> ()
  | Error message -> die message);
  let elapsed_ms = (Sys.time () -. start) *. 1000.0 in
  print_endline "";
  (* using printf here bc easier *)
  Printf.printf "Done! (%.3fms)\n" elapsed_ms
