
let () =
  let filename = Sys.argv.(1) in
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
    try
      let ast = Parser.goal Lexer.token lexbuf in  (* parse input *)
      let wast = Weeder.weed_program ast in
      let _ = Interpret.execute wast []
      in ()
    with
      | Failure msg        -> print_endline ("Failure in " ^ msg)
      | End_of_file        -> print_endline "Parse error: unexpected end of string"
      | Parsing.Parse_error -> print_endline "Parse error"
