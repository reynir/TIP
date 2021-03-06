type identifier = string

type binop =
  | LazyOr
  | LazyAnd
  | EagerOr
  | ExclusiveOr
  | EagerAnd
  | Equal
  | NotEqual
  | Lt
  | Gt
  | Le
  | Ge
  | Plus
  | Minus
  | Mul
  | Div
  | Mod

type expression =
  | Variable of identifier
  | IntegerLiteral of string
  | Null
  | CharLiteral of string
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
