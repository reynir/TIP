(* lexer.mll -*- tuareg -*- *)
{
  open Parser
  
  let get = Lexing.lexeme
    
  let keyword_table = Hashtbl.create 53

  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
       "boolean"     , BOOLEAN;
       "break"       , BREAK;
       "case"        , CASE;   
       "continue"    , CONTINUE;
       "default"     , DEFAULT;
       "do"          , DO;
       "else"        , ELSE;  
       "for"         , FOR;   
       "goto"        , GOTO;
       "if"          , IF;
       "int"         , INT;
       "return"      , RETURN;
       "switch"      , SWITCH;
       "while"       , WHILE;
       "true"        , TRUE;
       "false"       , FALSE;
       "null"        , NULL;
       "malloc"      , MALLOC;
       "output"      , OUTPUT;
       "var"         , VAR;
       "input"       , INPUT ]


}

(*******************************************************************
 * Helpers                                                         *
 *******************************************************************)

let latin1_input_character = ['\000'- '\255']
let ht = '\t'
let lf = '\n'
let ff = '\012'
let cr = '\r'
let sp = ' '

let line_terminator = lf | cr | cr lf 
let input_character = ['\000'-'\255'] # ['\r' '\n'] (* # (lf | cr) *)

let not_star_not_newline = (['\000'-'\255'] # ['\r''\n''*'])
let not_star_not_slash_not_newline = (['\000'-'\255'] # ['\r''\n''*''/'])

let digit = ['0'-'9']
let non_zero_digit = ['1'-'9']
let octal_digit = ['0'-'7']
let zero_to_three = ['0'-'3']
let lr_bracket = '[' (ht | lf | ff | cr | sp)* ']'

let decimal_numeral = '0' | non_zero_digit digit*

let latin1_letter =
       ['A'-'Z'] | ['a'-'z'] | ['\170'-'\170'] | ['\181'-'\181'] |
       ['\186'-'\186'] | ['\192'-'\214'] | ['\216'-'\246'] | ['\248'-'\255']

let java_letter = latin1_letter | '$' | '_'
let java_letter_or_digit = latin1_letter | digit | '$' | '_'

let octal_escape = '\\' (octal_digit octal_digit? | zero_to_three octal_digit octal_digit)
let escape_sequence = "\\b" | "\\t" | "\\n" | "\\f" | "\\r" | "\\" '"' | "\\" "'" | "\\\\" | octal_escape
let single_character = (['\000'-'\255'] # ['\r''\n'''''\\']) | escape_sequence
let string_character = (['\000'-'\255'] # ['\r''\n''"''\\']) | escape_sequence

(*******************************************************************
 * Tokens                                                          *
 *******************************************************************)

rule token = parse
  | eof    { EOF }
(* Whitespace *)
  | (sp | ht | ff )                 { token lexbuf }  (* white_space *)
  | line_terminator                 { Lexing.new_line lexbuf; token lexbuf }
  | "/*" line_terminator            { inside_comment false lexbuf }
  | "/*" not_star_not_newline       { inside_comment false lexbuf }
  | "/**"                           { inside_comment true lexbuf }
  | "//" input_character*
                                    { token lexbuf } (* end_of_line_comment *)
  | "//" input_character* line_terminator
                                    { Lexing.new_line lexbuf; token lexbuf } (* end_of_line_comment *)


(* Delimiters *)
  | '('             { L_PAREN }
  | ')'             { R_PAREN }
  | '{'             { L_BRACE }
  | '}'             { R_BRACE }
  | '['             { L_BRACKET }
  | ']'             { R_BRACKET }
  | ';'             { SEMICOLON }
  | ','             { COMMA }
  | '.'             { DOT }

(* Assignment and logic *)
  | '='             { ASSIGN }
  | '!'             { COMPLEMENT }
  | "&&"            { AND_AND }
  | "||"            { OR_OR }

(* Comparison *)
  | '<'             { LT }
  | '>'             { GT }
  | "=="            { EQ }
  | "<="            { LTEQ }
  | ">="            { GTEQ }
  | "!="            { NEQ }

(* Arithmetic *)
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { DIV }
  | '&'             { AND }
  | '|'             { OR }
  | '^'             { XOR }
  | '%'             { MOD }
  | "++"            { PLUS_PLUS }
  | "--"            { MINUS_MINUS }

(* Literals and identifiers *)
  | decimal_numeral as i                    { INTEGER_LITERAL i }
  | "'" single_character "'" as s           { CHAR_LITERAL s }
  | '"' string_character* '"' as s          { STRING_LITERAL s }
  | java_letter java_letter_or_digit* as id { try
						Hashtbl.find keyword_table id
                                              with Not_found ->
						IDENTIFIER id }

(* Two-state finite automata for recognizing end-of-comment
    bool laststar represents the state of the automata      *)
and inside_comment laststar = parse
  | '/'                            { if laststar
                                     then token lexbuf
                                     else inside_comment false lexbuf }
  | '*'                            { inside_comment true lexbuf }
  | line_terminator                { Lexing.new_line lexbuf;
				     inside_comment false lexbuf }
  | not_star_not_slash_not_newline { inside_comment false lexbuf }
