
let () =
  let lexbuf = Lexing.from_channel stdin in
    try
      let ast = Parser.goal Lexer.token lexbuf in  (* parse input *)
        Astpp.pp_program ast
    with
      | Failure msg        -> print_endline ("Failure in " ^ msg)
      | End_of_file        -> print_endline "Parse error: unexpected end of string"
      | Parsing.Parse_error -> print_endline "Parse error"
