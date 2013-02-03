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
%type <unit> goal
%%

goal 
  :  program EOF
     { }
;

program 
  : function_decl
    {}
  | function_decl program
    {}
  ;

function_decl
  : IDENTIFIER formals function_body
    {}
  ;

formals
  : L_PAREN R_PAREN
    {}
  | L_PAREN decl_list R_PAREN
    {}
  ;

decl_list
  : IDENTIFIER
    {}
  | IDENTIFIER COMMA decl_list
    {}
  ;

function_body
  : block
    {}
  ;

/* ********** Blocks and statements ********** */

block
  :  L_BRACE statement_or_declarations R_BRACE
     {}
  ;

local_variable_declaration
  : VAR decl_list SEMICOLON
    {}
  ;

statement_or_declarations
  :
    {}
  |  statement_or_declaration statement_or_declarations
    {}
  ;

statement_or_declaration
  : local_variable_declaration
     {}
  | if_then_statement
    {}
  | if_then_else_statement
    {}
  | while_statement
    {}
  | block
    {}
  | assignment_statement
    {}
  | output_statement
    {}
  | return_statement
    {}
  ;


output_statement
  : OUTPUT expression SEMICOLON
    {}
  ;

assignment_statement
  : IDENTIFIER ASSIGN expression SEMICOLON
    {}
  | STAR pointer_expression ASSIGN expression SEMICOLON
    {}
  ;

return_statement
  :  RETURN SEMICOLON
     {}
  |  RETURN expression SEMICOLON
     {}
  ;

if_then_statement
  :  IF L_PAREN expression R_PAREN block
     {}
  ;

if_then_else_statement
  :  IF L_PAREN expression R_PAREN block ELSE block
     {}
  ;

while_statement
  :  WHILE L_PAREN expression R_PAREN block
     {}
  ;

/* ********** Literals and names ********** */

literal 
  :  INTEGER_LITERAL
     {}
  |  CHAR_LITERAL
     {}
  |  boolean_literal
     {}
  |  STRING_LITERAL
     {}
  |  null_literal
     {}
  ;

boolean_literal
  :  TRUE
     {}
  |  FALSE
     {}
  ;

null_literal
  :  NULL
     {}
  ;

/* ********** Expressions ********** */

primary
  : IDENTIFIER
    {}
  | literal
    {}
  | L_PAREN expression R_PAREN
    {}
  | function_invocation
    {}
  | MALLOC
    {}
  ;

argument_list
  :
     {}
  |  argument_list_nonempty
     {}
  ;

argument_list_nonempty
  :  expression
     {}
  |  argument_list_nonempty COMMA expression
     {}
  ;

function_invocation
  : IDENTIFIER L_PAREN argument_list R_PAREN
    {}
  | L_PAREN expression R_PAREN L_PAREN argument_list R_PAREN
    {}
  ;

pointer_expression
  : primary
    {}
  | STAR pointer_expression
    {}
  | AND IDENTIFIER
    {}
  ;

unary_expression
  : pointer_expression
    {}
  | COMPLEMENT unary_expression
    {}
  ;

multiplicative_expression
  : unary_expression
    {}
  | multiplicative_expression STAR unary_expression
    {}
  | multiplicative_expression DIV unary_expression
    {}
  | multiplicative_expression MOD unary_expression
    {}
  ;

additive_expression
  :  multiplicative_expression
     {}
  |  additive_expression PLUS multiplicative_expression
     {}
  |  additive_expression MINUS multiplicative_expression
     {}
  ;

relational_expression
  : additive_expression
    {}
  | relational_expression LT additive_expression
    {}
  | relational_expression GT additive_expression
    {}
  | relational_expression LTEQ additive_expression
    {}
  | relational_expression GTEQ additive_expression
    {}
  ;

equality_expression
  : relational_expression
    {}
  | equality_expression EQ relational_expression
    {}
  | equality_expression NEQ relational_expression
    {}
  ;

and_expression
  :  equality_expression
     {}
  |  and_expression AND equality_expression
     {}
  ;

exclusive_or_expression
  :  and_expression
     {}
  |  exclusive_or_expression XOR and_expression
     {}
  ;

inclusive_or_expression
  :  exclusive_or_expression
     {}
  |  inclusive_or_expression OR exclusive_or_expression
     {}
  ;

lazy_and_expression
  :  inclusive_or_expression
     {}
  |  lazy_and_expression AND_AND inclusive_or_expression
     {}
  ;

lazy_or_expression
  :  lazy_and_expression
     {}
  |  lazy_or_expression OR_OR lazy_and_expression
     {}
  ;

expression
  :  lazy_or_expression
     {}
  ;
