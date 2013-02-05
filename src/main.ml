
let () =
  let lexbuf = Lexing.from_channel stdin in
    try
      let ast = Parser.goal Lexer.token lexbuf in  (* parse input *)
      let () = Astpp.pp_program ast in
      let wast = Weeder.weed_program ast in
      let () = Wastpp.pp_program wast in
      let _ = Interpret.execute wast []
      in ()
    with
      | Failure msg        -> print_endline ("Failure in " ^ msg)
      | End_of_file        -> print_endline "Parse error: unexpected end of string"
      | Parsing.Parse_error -> print_endline "Parse error"
