let string_to_char c =
  let oct_to_dec i =
    let p1 = i mod 10 in
    let p2 = (i/10) mod 10 in
    let p3 = (i/100) mod 10 in
      p1+8*p2+64*p3
  in
    if 1 == String.length c then c.[0]
    else if 2 == String.length c then
      match c.[1] with
        | 'b' -> '\b'
        | 't' -> '\t'
        | 'n' -> '\n'
        | 'f' -> '\012'
        | 'r' -> '\r'
        | c ->
            begin
              if c >= '0' && c <= '7' then Char.chr (Char.code c - (Char.code '0'))
              else c
            end
            else Char.chr (oct_to_dec (int_of_string (String.sub c 1 ((String.length c) - 1))))

let weed_char c =
  Char.code (string_to_char (String.sub c 1 ((String.length c) - 2)))

let rec weed_expression = function
  | Ast.Variable id -> Wast.Variable id
  | Ast.IntegerLiteral i -> Wast.IntegerLiteral (Z.of_string i)
  | Ast.CharLiteral c -> 
      Wast.IntegerLiteral (Z.of_int (weed_char c))
  | Ast.Binop (e1, op, e2) ->
      Wast.Binop (weed_expression e1, op, weed_expression e2)
  | Ast.Complement e ->
      Wast.Complement (weed_expression e)
  | Ast.Deref e ->
      Wast.Deref (weed_expression e)
  | Ast.Ref id ->
      Wast.Ref id
  | Ast.StaticInvoke (id, args) ->
      Wast.StaticInvoke (id, List.map weed_expression args)
  | Ast.NonstaticInvoke (e, args) ->
      Wast.NonstaticInvoke (weed_expression e, List.map weed_expression args)
  | Ast.Null -> Wast.Null
  | Ast.True -> Wast.IntegerLiteral Z.one
  | Ast.False -> Wast.IntegerLiteral Z.one
  | Ast.Malloc -> Wast.Malloc
  | Ast.Input -> Wast.Input

let rec weed_statement = function
  | Ast.LocalDecls ids ->
      Wast.LocalDecls ids
  | Ast.IfThen (test, consequent) ->
      Wast.IfThen (weed_expression test, weed_block consequent)
  | Ast.IfThenElse (test, consequent, alternative) ->
      Wast.IfThenElse (weed_expression test
                       , weed_block consequent
                       , weed_block alternative)
  | Ast.While (test, body) ->
      Wast.While (weed_expression test, weed_block body)
  | Ast.Block b -> Wast.Block (weed_block b)
  | Ast.VariableAssignment (v, e) ->
      Wast.VariableAssignment (v, weed_expression e)
  | Ast.PointerAssignment (e1, e2) ->
      Wast.PointerAssignment (weed_expression e1, weed_expression e2)
  | Ast.Output e ->
      Wast.Output (weed_expression e)
  | Ast.ValueReturn e ->
      Wast.ValueReturn (weed_expression e)
  | Ast.VoidReturn -> Wast.VoidReturn

and weed_block b =
  List.map weed_statement b



let weed_func (name, params, body) =
  (name, params, weed_block body)

let weed_program (funcs, main) =
  (List.map weed_func funcs, weed_func main)
