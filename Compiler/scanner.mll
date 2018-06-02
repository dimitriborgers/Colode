(* Ocamllex scanner for Colode *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let char_lex = ['\x00' - '\x7F']
let string_lex = char_lex+
let strings = '"' ( [^ '\\' '"'] | '\\' [^ '\n'] ) * '"'  

rule token = parse
  [' ' '\r' '\t' '\n'] { token lexbuf } (* Whitespace *)
| "//"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACE }
| ']'      { RBRACE }
| '{'      { LBLOCK }
| '}'      { RBLOCK }
| ';'      { SEMI }
| '|'	   { PIPE }
| ','      { COMMA }
| ':'      { COLON }
| '.'	   { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { EXPONENT }
| '%'      { MODULUS }
| '~'		{ TILDE }
| '='      { ASSIGN }
| "+="     { ASSIGNADD }
| "-="     { ASSIGNMINUS }
| "*="     { ASSIGNTIMES }
| "/="     { ASSIGNDIVIDE }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "->"     { ARROW }
| "**"     { CONV }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "if"     { IF }
| "in"     { IN }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "def"	   { DEF }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "char"   { CHAR }
| "string" { STRING }
| "list"   { LIST }
| "void"   { VOID }
| "Image"  { IMAGE }
| "Pixel"  { PIXEL }
| "matrix" { MATRIX }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| strings as s { LITERALSTRING(s) }
| '\'' (char_lex as c) '\'' { LITERALCHAR(c) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
