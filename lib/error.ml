exception Fault of string

let fail line fmt =
  Printf.ksprintf
    (fun message -> Error (Printf.sprintf "line %d: %s" line message))
    fmt
