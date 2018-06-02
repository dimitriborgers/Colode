(* Semantically-checked abstract syntax tree types *)
open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SCharLiteral of char
  | SStringLiteral of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of sexpr * sexpr
  | SAssignAdd of sexpr * sexpr
  | SAssignMinus of sexpr * sexpr
  | SAssignTimes of sexpr * sexpr
  | SAssignDivide of sexpr * sexpr
  | SDeclAssign of typ * string * sexpr
  | SCall of string * sexpr list
  | SArray of sexpr list
  | SArray2D of sexpr list list 
  | SArrayIndex of sexpr * sexpr
  | SArray2DIndex of sexpr * sexpr * sexpr
  | SImageIndex of sexpr * string
  | SMemberAccess of sexpr * string list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SDeclare of typ * string

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }


type sprogram = sstmt list * sfunc_decl list

(* Pretty-printing functions *)


let rec string_of_sexpr (sex:sexpr) = match snd sex with 
    SLiteral(l) -> string_of_int l
  | SFliteral(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
  | SAssignAdd(v, e) -> string_of_sexpr v ^ " += " ^ string_of_sexpr e
  | SAssignMinus(v, e) -> string_of_sexpr v ^ " -= " ^ string_of_sexpr e
  | SAssignTimes(v, e) -> string_of_sexpr v ^ " *= " ^ string_of_sexpr e
  | SAssignDivide(v, e) -> string_of_sexpr v ^ " /= " ^ string_of_sexpr e
  | SDeclAssign(t, v, e) -> (string_of_typ t) ^ v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
     f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArray(l) -> "[" ^ (String.concat ", " (List.map string_of_sexpr l)) ^ "]"
  | SArrayIndex(a, b) -> string_of_sexpr a ^ "[" ^ string_of_sexpr b ^ "]"
  | SArray2DIndex(a, b, c) -> string_of_sexpr a ^ "[" ^ string_of_sexpr b ^ "]" ^ "[" ^ string_of_sexpr c ^ "]"
  | SImageIndex(ex, chan) -> string_of_sexpr ex ^ "->" ^ chan
  | SMemberAccess(a, b) -> string_of_sexpr a ^ "." ^String.concat "." b
  | SCharLiteral(c) -> "'" ^ Char.escaped c ^ "'"
  | SStringLiteral(s) -> "\"" ^ s ^ "\""
  | SNoexpr -> ""


let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
    string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, st) ->
    "for " ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; "  ^ string_of_sexpr e3 ^" "^ string_of_sstmt st
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SDeclare(t, s) -> (string_of_typ t) ^ " " ^ s


let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (stmts, funcs) =
  String.concat "" (List.map string_of_sstmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)

