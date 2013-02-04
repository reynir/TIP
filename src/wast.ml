type identifier =
    Ast.identifier

type binop =
    Ast.binop

type expression =
  | Variable of identifier
  | IntegerLiteral of Z.t (* NEW *)
  | Null
  | CharLiteral of Z.t (* NEW *)
  | True
  | False
  | Binop of expression * binop * expression
  | Complement of expression
  | Deref of expression
  | Ref of identifier
  | StaticInvoke of identifier * (expression list)
  | NonstaticInvoke of expression * (expression list)
  | Malloc
  | Input

type statement =
  | LocalDecls of identifier list
  | IfThen of expression * block
  | IfThenElse of expression * block * block
  | While of expression * block
  | Block of block
  | VariableAssignment of identifier * expression
  | PointerAssignment of expression * expression
  | Output of expression
  | ValueReturn of expression
  | VoidReturn
and block = statement list

type function_decl =
    identifier * identifier list * block

type program =
    function_decl list * function_decl
