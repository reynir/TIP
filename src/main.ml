
let () =
  let lexbuf = Lexing.from_channel stdin in
    try
      let _ = Parser.goal Lexer.token lexbuf in  (* parse input *)
        print_endline "It works!"
    with
      | Failure msg        -> print_endline ("Failure in " ^ msg)
      | End_of_file        -> print_endline "Parse error: unexpected end of string"
      | Parsing.Parse_error -> print_endline "Parse error"
