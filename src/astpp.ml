open Ast

let rec indent level =
  if level <> 0
  then begin print_string "  "; 
             indent (level-1) end
  else ()

let rec comma_seq pp_x = function
  | [] -> ()
  | [x] -> pp_x x
  | x :: xs -> pp_x x; print_string ", "; comma_seq pp_x xs

let pp_paren pp_x x =
  print_string "(";
  pp_x x;
  print_string ")"

let pp_seq pp_x xs =
  pp_paren (comma_seq pp_x) xs

let pp_binop op =
  print_string
    (" "
     ^ (match op with
          | LazyOr -> "||"
          | LazyAnd -> "&&"
          | EagerOr -> "|"
          | ExclusiveOr -> "^"
          | EagerAnd -> "&"
          | Equal -> "=="
          | NotEqual -> "!="
          | Lt -> "<"
          | Gt -> ">"
          | Le -> "<="
          | Ge -> ">="
          | Plus -> "+"
          | Minus -> "-"
          | Mul -> "*"
          | Div -> "/"
          | Mod -> "%")
     ^ " ")

let rec pp_expression e = 
  (* Match all cases so we can get help from the type checker if we add a new
   * production in the grammar. *)
  match e with
    | Binop (_, op, _) ->
        (match op with
           | LazyOr -> pp_lazy_or e
           | LazyAnd -> pp_lazy_and e
           | EagerOr -> pp_eager_or e
           | ExclusiveOr -> pp_exclusive_or e
           | EagerAnd -> pp_eager_and e
           | Equal | NotEqual -> pp_equality e
           | Lt | Gt | Le | Ge -> pp_relational e
           | Plus | Minus -> pp_additive e
           | Mul | Div | Mod -> pp_multiplicative e)
    | Complement _ -> pp_complement e
    | Deref _ | Ref _ -> pp_pointer e

    | Variable _
    | IntegerLiteral _ | CharLiteral _
    | Null | True | False
    | Malloc | Input 
    | StaticInvoke _ | NonstaticInvoke _ -> 
        pp_primary e
        
and pp_lazy_or = function
  | Binop (e1, (LazyOr as c), e2) ->
      pp_lazy_and e1;
      pp_binop c;
      pp_lazy_or e2
  | e -> pp_lazy_and e
and pp_lazy_and = function
  | Binop (e1, (LazyAnd as c), e2) ->
      pp_eager_or e1;
      pp_binop c;
      pp_lazy_and e2
  | e -> pp_eager_or e
and pp_eager_or = function
  | Binop (e1, (EagerOr as c), e2) ->
      pp_eager_or e1;
      pp_binop c;
      pp_eager_or e2
  | e -> pp_exclusive_or e
and pp_exclusive_or = function
  | Binop (e1, (ExclusiveOr as c), e2) ->
      pp_eager_and e1;
      pp_binop c;
      pp_exclusive_or e2
  | e -> pp_eager_and e
and pp_eager_and = function
  | Binop (e1, (EagerAnd as c), e2) ->
      pp_equality e1;
      pp_binop c;
      pp_eager_and e2
  | e -> pp_equality e
and pp_equality = function
  | Binop (e1, (Equal as c), e2)
  | Binop (e1, (NotEqual as c), e2) ->
      pp_relational e1;
      pp_binop c;
      pp_equality e2
  | e -> pp_relational e
and pp_relational = function
  | Binop (e1, (Lt as c), e2)
  | Binop (e1, (Le as c), e2)
  | Binop (e1, (Gt as c), e2)
  | Binop (e1, (Ge as c), e2) ->
      pp_additive e1;
      pp_binop c;
      pp_relational e2
  | e -> pp_additive e
and pp_additive = function
  | Binop (e1, (Plus as c), e2)
  | Binop (e1, (Minus as c), e2) ->
      pp_multiplicative e1;
      pp_binop c;
      pp_additive e2
  | e -> pp_multiplicative e
and pp_multiplicative = function
  | Binop (e1, (Mul as c), e2)
  | Binop (e1, (Div as c), e2)
  | Binop (e1, (Mod as c), e2) ->
      pp_complement e1;
      pp_binop c;
      pp_multiplicative e2
  | e -> pp_complement e
and pp_complement = function
  | Complement e ->
      print_string "!";
      pp_complement e
  | e -> pp_pointer e
and pp_pointer = function
  | Ref id ->
      print_string ("&"^id)
  | Deref e ->
      print_string "*";
      pp_pointer e
  | e -> pp_primary e
and pp_primary = function
  | Variable id -> print_string id
  | IntegerLiteral i -> print_string i
  | CharLiteral c -> print_string c
  | Null -> print_string "null"
  | True -> print_string "true"
  | False -> print_string "false"
  | Malloc -> print_string "malloc"
  | Input -> print_string "input"
  | StaticInvoke (id, args) ->
      print_string id;
      pp_seq pp_expression args
  | NonstaticInvoke (e, args) ->
      pp_seq pp_expression [e];
      pp_seq pp_expression args
  | e -> 
      pp_paren pp_expression e

let rec pp_statement level = function
  | LocalDecls ids ->
      print_string "var ";
      comma_seq print_string ids;
      print_endline ";"
  | IfThen (test, consequent) ->
      print_string "if (";
      pp_expression test;
      print_endline ") {";
      pp_block (level+1) consequent;
      indent level;
      print_endline "}"
  | IfThenElse (test, consequent, alternative) ->
      print_string "if (";
      pp_expression test;
      print_endline ") {";
      pp_block (level+1) consequent;
      indent level;
      print_endline "} else {";
      pp_block (level+1) alternative;
      indent level;
      print_endline "}"
  | While (test, body) ->
      print_string "while (";
      pp_expression test;
      print_endline ") {";
      pp_block (level+1) body;
      indent level;
      print_endline "}"
  | Block stms ->
      print_endline "{";
      pp_block (level+1) stms;
      indent level;
      print_endline "}"
  | VariableAssignment (id, expr) ->
      print_string id;
      print_string " = ";
      pp_expression expr;
      print_endline ";"
  | PointerAssignment (e1, e2) ->
      print_string "*";
      pp_pointer e1;
      print_string " = ";
      pp_expression e2;
      print_endline ";"
  | Output expr ->
      print_string "output ";
      pp_expression expr;
      print_endline ";"
  | ValueReturn expr ->
      print_string "return ";
      pp_expression expr;
      print_endline ";"
  | VoidReturn ->
      print_endline "return;"

and pp_block level stms =
  List.iter
    (fun stm ->
       indent level;
       pp_statement level stm)
    stms

let pp_function_decl (name, params, body) =
  print_string name;
  pp_seq print_string params;
  print_endline " {";
  pp_block 1 body;
  print_endline "}";
  print_newline()


let pp_program (funcs, main) =
  List.iter
    pp_function_decl
    (funcs @ [main])
    
