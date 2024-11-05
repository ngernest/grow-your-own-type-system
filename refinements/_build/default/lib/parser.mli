
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | STAR
  | SOME
  | SLASH
  | RPAREN
  | REC
  | RBRACKET
  | PLUS
  | PERCENT
  | OR
  | NOT
  | NE
  | MINUS
  | LT
  | LPAREN
  | LET
  | LE
  | LBRACKET
  | INT of (int)
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FUN
  | FORALL
  | FALSE
  | EQUALS
  | EQ
  | EOF
  | ELSE
  | COMMA
  | COLON
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val ty_forall_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.s_ty)

val ty_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.s_expr Expr.ty)

val expr_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.s_expr)
