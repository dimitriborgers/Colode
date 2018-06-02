/* Ocamlyacc parser for Colode */

%{ open Ast %}

%token SEQUENCE LBLOCK RBLOCK LPAREN RPAREN LBRACE RBRACE SEMI COMMA PLUS MINUS TIMES DIVIDE
%token EXPONENT MODULUS ASSIGN ASSIGNADD ASSIGNMINUS ASSIGNTIMES ASSIGNDIVIDE NOT EQ NEQ LT LEQ GT GEQ AND OR DOT
%token RETURN IF ELSE ELIF FOR WHILE BREAK CONTINUE DEF INT BOOL FLOAT VOID IN
%token CHAR STRING LIST IMAGE PIXEL MATRIX COLON CONV PIPE TILDE POWER ARROW
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT LITERALSTRING
%token <char> LITERALCHAR
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE NOELIF ONED
%nonassoc ELSE ELIF  LBRACE LPAREN  RBRACE RPAREN
%right ASSIGN ASSIGNADD ASSIGNMINUS ASSIGNDIVIDE ASSIGNTIMES 
%left OR DOT PIPE
%left AND 
%left EQ NEQ POWER
%left LT GT LEQ GEQ
%left PLUS MINUS 
%left TIMES DIVIDE LEFT
%left EXPONENT MODULUS CONV ID SEMI 
%right NOT NEG TILDE


%%

program:
  decls EOF { let s, f = $1 in
    let s = List.rev s in (s, f) }

decls:
   /* nothing */ { ([], [])               }
 | decls stmt { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   DEF ID LPAREN formals_opt RPAREN COLON typ compound_stmt
     { { typ = $7;
	 fname = $2;
	 formals = $4;
	 locals = [];
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }
  | CHAR  { Char }
  | typ LIST  { ArrayList($1) }
  | STRING { String }
  | IMAGE  { Image }
  | PIXEL  { Pixel }
  | MATRIX { Matrix } 
/*
vdecl_list:
       { [] }
  | vdecl_list COMMA vdecl { $2 :: $1 }
*/
vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

compound_stmt:
 | LBLOCK stmt_list RBLOCK { $2 }
  
stmt:
   expr SEMI                           { Expr $1               }
  | RETURN expr_opt SEMI                        { Return $2             }
  | IF expr stmt %prec NOELSE { If($2, $3, Block([])) }
  | IF expr stmt ELIF expr stmt %prec NOELSE   { If($2, $3, If($5, $6, Block([])))  }
  | IF expr stmt ELIF expr stmt ELSE stmt  { If($2, $3, If($5, $6, $8))  }
  | IF expr stmt ELSE stmt  %prec NOELIF  { If($2, $3, $5)        }
  | FOR expr_opt SEMI expr SEMI expr_opt SEMI stmt
                                            { For($2, $4, $6, $8)   }
  | WHILE expr stmt           { While($2, $3)         }
  | vdecl                                   { Declare(fst $1, snd $1) }
  | compound_stmt {Block(List.rev $1)}

expr_opt:
  |  { Noexpr }
  | expr          { $1 }

/*member: 
 DOT ID { [$2] } 
 | member DOT ID { $3 :: $1 }
*/

array_index: ID DOT atom  { ArrayIndex(Id($1),$3) }

matrix_index:  ID DOT atom PIPE atom { Array2DIndex(Id($1), $3, $5)}

array_names:  array_index {$1}
    | matrix_index {$1}

image_index: ID ARROW ID { ImageIndex(Id($1), $3)}

name: ID {Id($1)}
    | array_names {$1}
    | image_index { $1 }

atom: LITERAL          { Literal($1)              }
  | FLIT       { Fliteral($1)             }
  | BLIT             { BoolLit($1)              }
  | LITERALCHAR      { CharLiteral($1)          }
  | LITERALSTRING    { StringLiteral(String.sub $1 1 ((String.length $1)-2))            } 
  | ID               { Id($1)                   }

term:  term PLUS   term { Binop($1, Add,   $3)     }
  | term MINUS  term { Binop($1, Sub,   $3)     }
  | term TIMES  term { Binop($1, Mult,  $3)     }
  | term EXPONENT  term { Binop($1, Exp,  $3)   }
  | term DIVIDE term { Binop($1, Div,   $3)     }
  | term EQ     term { Binop($1, Equal, $3)     }
  | term NEQ    term { Binop($1, Neq,   $3)     }
  | term LT     term { Binop($1, Less,  $3)     }
  | term LEQ    term { Binop($1, Leq,   $3)     }
  | term GT     term { Binop($1, Greater, $3)   }
  | term GEQ    term { Binop($1, Geq,   $3)     }
  | term AND    term { Binop($1, And,   $3)     }
  | term OR     term { Binop($1, Or,    $3)     }
  | term CONV     term { Binop($1, Conv,    $3) }
  | array_index  { $1 }
  | matrix_index { $1 }
  | image_index { $1 }
  | atom {$1}

expr: 
  NOT expr         { Unop(Not, $2)            }
  | name ASSIGN expr   { Assign($1, $3)         }
  | typ ID ASSIGN expr   { DeclAssign($1, $2, $4)   }
  | name ASSIGNADD expr   { AssignAdd($1, $3)       }
  | name ASSIGNMINUS expr   { AssignMinus($1, $3)   }
  | name ASSIGNTIMES expr   { AssignTimes($1, $3)   }
  | name ASSIGNDIVIDE expr   { AssignDivide($1, $3) }
  | ID LPAREN args_opt RPAREN { Call($1, $3)        }
  /*| LPAREN expr RPAREN { $2 } */
  | array_lit          { $1 }
  | matrix_lit        { $1 }
  /*| atom LBRACE atom RBRACE LBRACE atom RBRACE { Array2DIndex($1,$3, $6) }*/
  /*| ID member     { MemberAccess(Id($1), List.rev $2) }*/
  | term {$1}


array_lit: LBRACE array_opt RBRACE { Array(List.rev $2) }

array_opt: { [] } 
  | expr { [$1] }
  |  array_opt COMMA expr { $3 :: $1 }

matrix_lit: TILDE LBRACE rows RBRACE { Array2D(List.rev $3)}

rows:
 row { [List.rev $1] }
 | rows PIPE row { $3 :: (List.rev $1) }

row:
    LITERAL { [Fliteral(string_of_int $1)] }
  | FLIT { [Fliteral($1)]}
  | row FLIT { Fliteral($2) :: $1 }
  | row LITERAL { Fliteral(string_of_int $2) :: $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
