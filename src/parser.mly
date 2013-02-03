/* File parser.mly */
%{


%}
%token EOF
/* Keywords */
%token BOOLEAN
%token BREAK
%token CASE
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token FOR
%token GOTO
%token IF
%token INT
%token RETURN
%token SWITCH
%token VOID
%token WHILE
%token MALLOC
%token OUTPUT
%token VAR

%token TRUE FALSE
%token NULL

/* Delimiters */
%token L_PAREN R_PAREN
%token L_BRACE R_BRACE
%token L_BRACKET R_BRACKET
%token SEMICOLON
%token COMMA
%token DOT

/* Assignment and logic */
%token ASSIGN
%token COMPLEMENT
%token AND_AND OR_OR

/* Comparison */
%token LT GT EQ
%token LTEQ GTEQ NEQ

/* Arithmetic */
%token PLUS MINUS STAR DIV MOD
%token AND OR XOR
%token PLUS_PLUS MINUS_MINUS

/* Literals and identifiers */
%token <string>INTEGER_LITERAL
%token <string>CHAR_LITERAL
%token <string>STRING_LITERAL
%token <string>IDENTIFIER

%start goal             /* the entry point */
%type <Ast.source_file> goal
%%

goal 
  :  program EOF
     { Program $1 }
;

program 
  : function_decl program
    { $1 :: $2 }
  ;

function_decl
  : IDENTIFIER formals function_body
    { Function ($2, $3) }
  ;

function_body
  : block
    { $1 }
  ;

formals
  : L_PAREN decl_list R_PAREN
    { $2 }
  ;

decl_list
  : IDENTIFIER
    { [$1] }
  | IDENTIFIER COMMA decl_list
    { $1 :: $3 }
  ;

/* ********** Blocks and statements ********** */

block
  :  L_BRACE statement_or_declarations R_BRACE
     { List.rev $2 }
  ;

local_variable_declaration
  : VAR decl_list SEMICOLON
    { Ast.LocalDecls $2 }
  ;

statement_or_declarations
  :
    { [] }
  |  statement_or_declarations statement_or_declaration
    { $2 :: $1 }
  ;

statement_or_declaration
  :  statement
     { $1 }
  |  local_variable_declaration_statement
     { $1 }
  ;

statement
  : if_then_statement
    { $1 }
  | if_then_else_statement
    { $1 }
  | while_statement
    { $1 }
  | block
    { make_stm(Ast.Block $1) }
  | assignment_statement
    { $1 }
  | output_statement
    { $1 }
  | return_statement
    { $1 }
  ;

local_variable_declaration_statement
  :  local_variable_declaration SEMICOLON
     { let (ty,id,exp) = $1 in
       make_stm(Ast.LocalDecl (ty,id,exp)) }
  ;

output_statement
  : OUTPUT expression SEMICOLON
    { Output $2 }
  ;

assignment_statement
  : IDENTIFIER ASSIGN expression SEMICOLON
    { Assignment ($1,$3) }
  ;

return_statement
  :  RETURN SEMICOLON
     { make_stm(Ast.VoidReturn) }
  |  RETURN expression SEMICOLON
     { make_stm(Ast.ValueReturn $2) }
  ;

if_then_statement
  :  IF L_PAREN expression R_PAREN block
     { make_stm(Ast.IfThen($3,$5)) }
  ;

if_then_else_statement
  :  IF L_PAREN expression R_PAREN block ELSE block
     { make_stm(Ast.IfThenElse($3,$5,$7)) }
  ;

while_statement
  :  WHILE L_PAREN expression R_PAREN block
     { make_stm(Ast.While($3,$5)) }
  ;

/* ********** Literals and names ********** */

literal 
  :  INTEGER_LITERAL
     { make_exp(Ast.IntConst $1) }
  |  CHAR_LITERAL
     { make_exp(Ast.CharConst $1) }
  |  boolean_literal
     { make_exp(Ast.BooleanConst $1) }
  |  STRING_LITERAL
     { make_exp(Ast.StringConst $1) }
  |  null_literal
     { make_exp(Ast.Null) }
  ;

boolean_literal
  :  TRUE
     { true }
  |  FALSE
     { false }
  ;

null_literal
  :  NULL
     {}
  ;

/* ********** Expressions ********** */

primary
  : IDENTIFIER
    { make_exp(Ast.Lvalue($1)) }
  | literal
    { $1 }
  | L_PAREN expression R_PAREN
    { make_exp(Ast.Paren $2) }
  | function_invocation
    { $1 }
  | MALLOC
    { Ast.Malloc }
  ;

argument_list
  :
     { [] }
  |  argument_list_nonempty
     { List.rev $1 }
  ;

argument_list_nonempty
  :  expression
     { [$1] }
  |  argument_list_nonempty COMMA expression
     { $3 :: $1 }
  ;

function_invocation
  : IDENTIFIER L_PAREN argument_list R_PAREN
    { Ast.StaticInvoke ($1, $3) }
  | L_PAREN expression R_PAREN L_PAREN argument_list R_PAREN
    { Ast.NontaticInvoke ($2, $5) }
  ;

pointer_expression
  : primary
    { $1 }
  | STAR pointer_expression
    { Ast.Dereference $2 }
  | AND IDENTIFIER
    { Ast.Pointer $2 }
  ;

unary_expression
  : pointer_expression
    { $1 }
  | COMPLEMENT unary_expression
    { Ast.Complement $2 }
  ;

multiplicative_expression
  : unary_expression
    { $1 }
  | multiplicative_expression STAR pointer_expression
    { Ast.Binop($1,Ast.Times,$3) }
  | multiplicative_expression DIV pointer_expression
    { Ast.Binop($1,Ast.Divide,$3) }
  | multiplicative_expression MOD pointer_expression
    { Ast.Binop($1,Ast.Modulo,$3) }
  ;

additive_expression
  :  multiplicative_expression
     { $1 }
  |  additive_expression PLUS multiplicative_expression
     { make_exp(Ast.Binop($1,Ast.Plus,$3)) }
  |  additive_expression MINUS multiplicative_expression
     { make_exp(Ast.Binop($1,Ast.Minus,$3)) }
  ;

relational_expression
  : additive_expression
    { $1 }
  | relational_expression LT additive_expression
    { make_exp(Ast.Binop($1,Ast.Lt,$3)) }
  | relational_expression GT additive_expression
    { make_exp(Ast.Binop($1,Ast.Gt,$3)) }
  | relational_expression LTEQ additive_expression
    { make_exp(Ast.Binop($1,Ast.Le,$3)) }
  | relational_expression GTEQ additive_expression
    { make_exp(Ast.Binop($1,Ast.Ge,$3)) }
  ;

equality_expression
  : relational_expression
    { $1 }
  | equality_expression EQ relational_expression
    { make_exp(Ast.Binop($1,Ast.Eq,$3)) }
  | equality_expression NEQ relational_expression
    { make_exp(Ast.Binop($1,Ast.Ne,$3)) }
  ;

and_expression
  :  equality_expression
     { $1 }
  |  and_expression AND equality_expression
     { make_exp(Ast.Binop($1,Ast.And,$3)) }
  ;

exclusive_or_expression
  :  and_expression
     { $1 }
  |  exclusive_or_expression XOR and_expression
     { make_exp(Ast.Binop($1,Ast.Xor,$3)) }
  ;

inclusive_or_expression
  :  exclusive_or_expression
     { $1 }
  |  inclusive_or_expression OR exclusive_or_expression
     { make_exp(Ast.Binop($1,Ast.Or,$3)) }
  ;

lazy_and_expression
  :  inclusive_or_expression
     { $1 }
  |  lazy_and_expression AND_AND inclusive_or_expression
     { make_exp(Ast.Binop($1,Ast.LazyAnd,$3)) }
  ;

lazy_or_expression
  :  lazy_and_expression
     { $1 }
  |  lazy_or_expression OR_OR lazy_and_expression
     { make_exp(Ast.Binop($1,Ast.LazyOr,$3)) }
  ;

expression
  :  lazy_or_expression
     { $1 }
  ;
