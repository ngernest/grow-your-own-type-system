
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
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
    | INT of (
# 32 "lib/parser.mly"
       (int)
# 34 "lib/parser.ml"
  )
    | IN
    | IF
    | IDENT of (
# 31 "lib/parser.mly"
       (string)
# 41 "lib/parser.ml"
  )
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
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  

open Expr
open Infer

module StringMap = Map.Make (String)

let unary op arg = SCall(SVar op, [arg])
let binary op left right = SCall(SVar op, [left; right])

let replace_ty_constants_with_vars var_name_list ty =
	let env = List.fold_left
		(fun env var_name -> StringMap.add var_name (new_gen_var ()) env)
		StringMap.empty var_name_list
	in
	let rec f = function
		| TConst name as ty -> begin
				try StringMap.find name env
				with Not_found -> ty
			end
		| TApp(name, arg_ty_list) -> TApp(name, List.map f arg_ty_list)
		| TArrow(param_r_ty_list, return_r_ty) ->
				let g = r_ty_map f in
				TArrow(List.map g param_r_ty_list, g return_r_ty)
		| TVar _ as ty -> ty
	in
	f ty


# 91 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_expr_eof) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: expr_eof. *)

  | MenhirState002 : (('s, 'r) _menhir_cell1_NOT, 'r) _menhir_state
    (** State 002.
        Stack shape : NOT.
        Start symbol: <undetermined>. *)

  | MenhirState003 : (('s, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 003.
        Stack shape : MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState004 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 004.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState007 : (('s, 'r) _menhir_cell1_LET _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 007.
        Stack shape : LET IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState009 : (('s, 'r) _menhir_cell1_IF, 'r) _menhir_state
    (** State 009.
        Stack shape : IF.
        Start symbol: <undetermined>. *)

  | MenhirState012 : (('s, 'r) _menhir_cell1_FUN, 'r) _menhir_state
    (** State 012.
        Stack shape : FUN.
        Start symbol: <undetermined>. *)

  | MenhirState014 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_RPAREN, 'r) _menhir_state
    (** State 014.
        Stack shape : FUN RPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState016 : (('s, 'r) _menhir_cell1_SOME, 'r) _menhir_state
    (** State 016.
        Stack shape : SOME.
        Start symbol: <undetermined>. *)

  | MenhirState017 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 017.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState020 : ((('s, 'r) _menhir_cell1_SOME, 'r) _menhir_cell1_ident_list, 'r) _menhir_state
    (** State 020.
        Stack shape : SOME ident_list.
        Start symbol: <undetermined>. *)

  | MenhirState021 : (((('s, 'r) _menhir_cell1_SOME, 'r) _menhir_cell1_ident_list, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 021.
        Stack shape : SOME ident_list LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState023 : (('s, 'r) _menhir_cell1_SOME, 'r) _menhir_state
    (** State 023.
        Stack shape : SOME.
        Start symbol: <undetermined>. *)

  | MenhirState025 : ((('s, 'r) _menhir_cell1_SOME, 'r) _menhir_cell1_ident_list, 'r) _menhir_state
    (** State 025.
        Stack shape : SOME ident_list.
        Start symbol: <undetermined>. *)

  | MenhirState026 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 026.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState028 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_RPAREN, 'r) _menhir_state
    (** State 028.
        Stack shape : LPAREN RPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState029 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 029.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState031 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 031.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState036 : (('s, 'r) _menhir_cell1_ty, 'r) _menhir_state
    (** State 036.
        Stack shape : ty.
        Start symbol: <undetermined>. *)

  | MenhirState039 : (('s, 'r) _menhir_cell1_simple_ty, 'r) _menhir_state
    (** State 039.
        Stack shape : simple_ty.
        Start symbol: <undetermined>. *)

  | MenhirState043 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 043.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState045 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_ty, 'r) _menhir_state
    (** State 045.
        Stack shape : IDENT ty.
        Start symbol: <undetermined>. *)

  | MenhirState048 : (('s, 'r) _menhir_cell1_simple_expr, 'r) _menhir_state
    (** State 048.
        Stack shape : simple_expr.
        Start symbol: <undetermined>. *)

  | MenhirState051 : (('s, 'r) _menhir_cell1_relation_expr, 'r) _menhir_state
    (** State 051.
        Stack shape : relation_expr.
        Start symbol: <undetermined>. *)

  | MenhirState055 : (('s, 'r) _menhir_cell1_arithmetic_expr, 'r) _menhir_state
    (** State 055.
        Stack shape : arithmetic_expr.
        Start symbol: <undetermined>. *)

  | MenhirState057 : (('s, 'r) _menhir_cell1_arithmetic_expr, 'r) _menhir_state
    (** State 057.
        Stack shape : arithmetic_expr.
        Start symbol: <undetermined>. *)

  | MenhirState059 : (('s, 'r) _menhir_cell1_arithmetic_expr, 'r) _menhir_state
    (** State 059.
        Stack shape : arithmetic_expr.
        Start symbol: <undetermined>. *)

  | MenhirState061 : (('s, 'r) _menhir_cell1_arithmetic_expr, 'r) _menhir_state
    (** State 061.
        Stack shape : arithmetic_expr.
        Start symbol: <undetermined>. *)

  | MenhirState064 : (('s, 'r) _menhir_cell1_arithmetic_expr, 'r) _menhir_state
    (** State 064.
        Stack shape : arithmetic_expr.
        Start symbol: <undetermined>. *)

  | MenhirState071 : (('s, 'r) _menhir_cell1_arithmetic_expr _menhir_cell0_relation_op, 'r) _menhir_state
    (** State 071.
        Stack shape : arithmetic_expr relation_op.
        Start symbol: <undetermined>. *)

  | MenhirState073 : (('s, 'r) _menhir_cell1_relation_expr, 'r) _menhir_state
    (** State 073.
        Stack shape : relation_expr.
        Start symbol: <undetermined>. *)

  | MenhirState079 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 079.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState082 : (('s, 'r) _menhir_cell1_simple_expr, 'r) _menhir_state
    (** State 082.
        Stack shape : simple_expr.
        Start symbol: <undetermined>. *)

  | MenhirState084 : ((('s, 'r) _menhir_cell1_simple_expr, 'r) _menhir_cell1_ty, 'r) _menhir_state
    (** State 084.
        Stack shape : simple_expr ty.
        Start symbol: <undetermined>. *)

  | MenhirState091 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_refined_ty, 'r) _menhir_state
    (** State 091.
        Stack shape : LPAREN refined_ty.
        Start symbol: <undetermined>. *)

  | MenhirState094 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_param_ty, 'r) _menhir_state
    (** State 094.
        Stack shape : LPAREN param_ty.
        Start symbol: <undetermined>. *)

  | MenhirState099 : (((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_param_ty, 'r) _menhir_cell1_param_ty_list, 'r) _menhir_state
    (** State 099.
        Stack shape : LPAREN param_ty param_ty_list.
        Start symbol: <undetermined>. *)

  | MenhirState102 : ((('s, 'r) _menhir_cell1_param_ty, 'r) _menhir_cell1_param_ty, 'r) _menhir_state
    (** State 102.
        Stack shape : param_ty param_ty.
        Start symbol: <undetermined>. *)

  | MenhirState110 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 110.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState112 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 112.
        Stack shape : LPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState115 : (((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_ty, 'r) _menhir_state
    (** State 115.
        Stack shape : LPAREN IDENT ty.
        Start symbol: <undetermined>. *)

  | MenhirState121 : (((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_RPAREN, 'r) _menhir_cell1_return_ty, 'r) _menhir_state
    (** State 121.
        Stack shape : FUN RPAREN return_ty.
        Start symbol: <undetermined>. *)

  | MenhirState123 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_RPAREN, 'r) _menhir_state
    (** State 123.
        Stack shape : FUN RPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState126 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 126.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState128 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_ty, 'r) _menhir_state
    (** State 128.
        Stack shape : IDENT ty.
        Start symbol: <undetermined>. *)

  | MenhirState132 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_param_list, 'r) _menhir_state
    (** State 132.
        Stack shape : FUN param_list.
        Start symbol: <undetermined>. *)

  | MenhirState134 : (((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_param_list, 'r) _menhir_cell1_return_ty, 'r) _menhir_state
    (** State 134.
        Stack shape : FUN param_list return_ty.
        Start symbol: <undetermined>. *)

  | MenhirState136 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_param_list, 'r) _menhir_state
    (** State 136.
        Stack shape : FUN param_list.
        Start symbol: <undetermined>. *)

  | MenhirState139 : (('s, 'r) _menhir_cell1_param, 'r) _menhir_state
    (** State 139.
        Stack shape : param.
        Start symbol: <undetermined>. *)

  | MenhirState142 : (('s, 'r) _menhir_cell1_FUN _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 142.
        Stack shape : FUN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState145 : ((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 145.
        Stack shape : IF expr.
        Start symbol: <undetermined>. *)

  | MenhirState147 : (((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_expr, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 147.
        Stack shape : IF expr expr.
        Start symbol: <undetermined>. *)

  | MenhirState150 : ((('s, 'r) _menhir_cell1_LET _menhir_cell0_IDENT, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 150.
        Stack shape : LET IDENT expr.
        Start symbol: <undetermined>. *)

  | MenhirState159 : ('s, _menhir_box_ty_eof) _menhir_state
    (** State 159.
        Stack shape : .
        Start symbol: ty_eof. *)

  | MenhirState163 : ('s, _menhir_box_ty_forall_eof) _menhir_state
    (** State 163.
        Stack shape : .
        Start symbol: ty_forall_eof. *)

  | MenhirState165 : (('s, _menhir_box_ty_forall_eof) _menhir_cell1_FORALL, _menhir_box_ty_forall_eof) _menhir_state
    (** State 165.
        Stack shape : FORALL.
        Start symbol: ty_forall_eof. *)

  | MenhirState167 : ((('s, _menhir_box_ty_forall_eof) _menhir_cell1_FORALL, _menhir_box_ty_forall_eof) _menhir_cell1_ident_list, _menhir_box_ty_forall_eof) _menhir_state
    (** State 167.
        Stack shape : FORALL ident_list.
        Start symbol: ty_forall_eof. *)


and ('s, 'r) _menhir_cell1_arithmetic_expr = 
  | MenhirCell1_arithmetic_expr of 's * ('s, 'r) _menhir_state * (Expr.s_expr)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Expr.s_expr)

and ('s, 'r) _menhir_cell1_ident_list = 
  | MenhirCell1_ident_list of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_param = 
  | MenhirCell1_param of 's * ('s, 'r) _menhir_state * (Expr.s_param)

and ('s, 'r) _menhir_cell1_param_list = 
  | MenhirCell1_param_list of 's * ('s, 'r) _menhir_state * (Expr.s_param list)

and ('s, 'r) _menhir_cell1_param_ty = 
  | MenhirCell1_param_ty of 's * ('s, 'r) _menhir_state * (Expr.s_expr Expr.refined_ty)

and ('s, 'r) _menhir_cell1_param_ty_list = 
  | MenhirCell1_param_ty_list of 's * ('s, 'r) _menhir_state * (Expr.s_expr Expr.refined_ty list)

and ('s, 'r) _menhir_cell1_refined_ty = 
  | MenhirCell1_refined_ty of 's * ('s, 'r) _menhir_state * (Expr.s_expr Expr.refined_ty)

and ('s, 'r) _menhir_cell1_relation_expr = 
  | MenhirCell1_relation_expr of 's * ('s, 'r) _menhir_state * (Expr.s_expr)

and 's _menhir_cell0_relation_op = 
  | MenhirCell0_relation_op of 's * (string)

and ('s, 'r) _menhir_cell1_return_ty = 
  | MenhirCell1_return_ty of 's * ('s, 'r) _menhir_state * (Expr.s_expr Expr.refined_ty)

and ('s, 'r) _menhir_cell1_simple_expr = 
  | MenhirCell1_simple_expr of 's * ('s, 'r) _menhir_state * (Expr.s_expr)

and ('s, 'r) _menhir_cell1_simple_ty = 
  | MenhirCell1_simple_ty of 's * ('s, 'r) _menhir_state * (Expr.s_expr Expr.ty)

and ('s, 'r) _menhir_cell1_ty = 
  | MenhirCell1_ty of 's * ('s, 'r) _menhir_state * (Expr.s_expr Expr.ty)

and ('s, 'r) _menhir_cell1_FORALL = 
  | MenhirCell1_FORALL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 31 "lib/parser.mly"
       (string)
# 432 "lib/parser.ml"
)

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 31 "lib/parser.mly"
       (string)
# 439 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RPAREN = 
  | MenhirCell1_RPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SOME = 
  | MenhirCell1_SOME of 's * ('s, 'r) _menhir_state

and _menhir_box_ty_forall_eof = 
  | MenhirBox_ty_forall_eof of (Expr.s_ty) [@@unboxed]

and _menhir_box_ty_eof = 
  | MenhirBox_ty_eof of (Expr.s_expr Expr.ty) [@@unboxed]

and _menhir_box_expr_eof = 
  | MenhirBox_expr_eof of (Expr.s_expr) [@@unboxed]

let _menhir_action_03 =
  fun _1 ->
    (
# 82 "lib/parser.mly"
                                             ( _1 )
# 477 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_04 =
  fun _2 ->
    (
# 83 "lib/parser.mly"
                                             ( unary "unary-" _2 )
# 485 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_05 =
  fun _1 _3 ->
    (
# 84 "lib/parser.mly"
                                             ( binary "+" _1 _3 )
# 493 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_06 =
  fun _1 _3 ->
    (
# 85 "lib/parser.mly"
                                             ( binary "-" _1 _3 )
# 501 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_07 =
  fun _1 _3 ->
    (
# 86 "lib/parser.mly"
                                             ( binary "*" _1 _3 )
# 509 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_08 =
  fun _1 _3 ->
    (
# 87 "lib/parser.mly"
                                             ( binary "/" _1 _3 )
# 517 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_09 =
  fun _1 _3 ->
    (
# 88 "lib/parser.mly"
                                             ( binary "%" _1 _3 )
# 525 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_10 =
  fun _1 ->
    (
# 72 "lib/parser.mly"
                                     ( _1 )
# 533 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_11 =
  fun _2 ->
    (
# 73 "lib/parser.mly"
                                     ( unary "not" _2 )
# 541 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_12 =
  fun _1 _3 ->
    (
# 74 "lib/parser.mly"
                                     ( binary "and" _1 _3 )
# 549 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_13 =
  fun _1 _3 ->
    (
# 75 "lib/parser.mly"
                                     ( binary "or" _1 _3 )
# 557 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_14 =
  fun _1 ->
    (
# 64 "lib/parser.mly"
                                               ( _1 )
# 565 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_15 =
  fun _1 _3 ->
    (
# 65 "lib/parser.mly"
                                               ( SCast(_1, _3, None) )
# 573 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_16 =
  fun _1 _3 _5 ->
    (
# 66 "lib/parser.mly"
                                               ( SCast(_1, _3, Some _5) )
# 581 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_17 =
  fun _2 _4 _6 ->
    (
# 67 "lib/parser.mly"
                                               ( SLet(_2, _4, _6) )
# 589 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_18 =
  fun _1 ->
    (
# 68 "lib/parser.mly"
                                               ( _1 )
# 597 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_19 =
  fun _2 _4 _6 ->
    (
# 69 "lib/parser.mly"
                                               ( SIf(_2, _4, _6) )
# 605 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_20 =
  fun _1 ->
    (
# 100 "lib/parser.mly"
                                 ( [_1] )
# 613 "lib/parser.ml"
     : (Expr.s_expr list))

let _menhir_action_21 =
  fun _1 _3 ->
    (
# 101 "lib/parser.mly"
                                 ( _1 :: _3 )
# 621 "lib/parser.ml"
     : (Expr.s_expr list))

let _menhir_action_22 =
  fun _1 ->
    (
# 55 "lib/parser.mly"
                   ( _1 )
# 629 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_23 =
  fun _2 _4 ->
    (
# 112 "lib/parser.mly"
                                                             ( SFun([(_2, None)], None, _4) )
# 637 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_24 =
  fun _3 _6 ->
    (
# 113 "lib/parser.mly"
                                                             ( SFun(_3, None, _6) )
# 645 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_25 =
  fun _5 ->
    (
# 114 "lib/parser.mly"
                                                             ( SFun([], None, _5) )
# 653 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_26 =
  fun _3 _6 _8 ->
    (
# 115 "lib/parser.mly"
                                                             ( SFun(_3, Some _6, _8) )
# 661 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_27 =
  fun _5 _7 ->
    (
# 116 "lib/parser.mly"
                                                             ( SFun([], Some _5, _7) )
# 669 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_28 =
  fun _1 ->
    (
# 154 "lib/parser.mly"
                                 ( Plain _1 )
# 677 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_29 =
  fun _2 ->
    (
# 155 "lib/parser.mly"
                                 ( _2 )
# 685 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_30 =
  fun _4 ->
    (
# 148 "lib/parser.mly"
                                                                     ( TArrow([], _4) )
# 693 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_31 =
  fun _1 _3 ->
    (
# 149 "lib/parser.mly"
                                                                     ( TArrow([Plain _1], _3) )
# 701 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_32 =
  fun _2 _5 ->
    (
# 150 "lib/parser.mly"
                                                                     ( TArrow([_2], _5) )
# 709 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_33 =
  fun _2 _4 _7 ->
    (
# 151 "lib/parser.mly"
                                                                     ( TArrow(_2 :: _4, _7) )
# 717 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_34 =
  fun _1 ->
    (
# 133 "lib/parser.mly"
                       ( [_1] )
# 725 "lib/parser.ml"
     : (string list))

let _menhir_action_35 =
  fun _1 _2 ->
    (
# 134 "lib/parser.mly"
                       ( _1 :: _2 )
# 733 "lib/parser.ml"
     : (string list))

let _menhir_action_36 =
  fun _1 ->
    (
# 123 "lib/parser.mly"
                               ( (_1, None) )
# 741 "lib/parser.ml"
     : (Expr.s_param))

let _menhir_action_37 =
  fun _1 _3 ->
    (
# 124 "lib/parser.mly"
                               ( (_1, Some (_3, None)) )
# 749 "lib/parser.ml"
     : (Expr.s_param))

let _menhir_action_38 =
  fun _1 _3 _5 ->
    (
# 125 "lib/parser.mly"
                               ( (_1, Some (_3, Some _5)) )
# 757 "lib/parser.ml"
     : (Expr.s_param))

let _menhir_action_39 =
  fun _1 ->
    (
# 119 "lib/parser.mly"
                           ( [_1] )
# 765 "lib/parser.ml"
     : (Expr.s_param list))

let _menhir_action_40 =
  fun _1 _3 ->
    (
# 120 "lib/parser.mly"
                           ( _1 :: _3 )
# 773 "lib/parser.ml"
     : (Expr.s_param list))

let _menhir_action_41 =
  fun _1 ->
    (
# 162 "lib/parser.mly"
                             ( Plain _1 )
# 781 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_42 =
  fun _1 ->
    (
# 163 "lib/parser.mly"
                             ( _1 )
# 789 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_43 =
  fun _1 ->
    (
# 158 "lib/parser.mly"
                                   ( [_1] )
# 797 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty list))

let _menhir_action_44 =
  fun _1 _3 ->
    (
# 159 "lib/parser.mly"
                                   ( _1 :: _3 )
# 805 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty list))

let _menhir_action_45 =
  fun _1 _3 ->
    (
# 166 "lib/parser.mly"
                             ( Named(_1, _3) )
# 813 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_46 =
  fun _1 _3 _5 ->
    (
# 167 "lib/parser.mly"
                             ( Refined(_1, _3, _5) )
# 821 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_47 =
  fun _1 ->
    (
# 78 "lib/parser.mly"
                                                     ( _1 )
# 829 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_48 =
  fun _1 _2 _3 ->
    (
# 79 "lib/parser.mly"
                                                     ( binary _2 _1 _3 )
# 837 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_49 =
  fun () ->
    (
# 104 "lib/parser.mly"
         ( "<" )
# 845 "lib/parser.ml"
     : (string))

let _menhir_action_50 =
  fun () ->
    (
# 105 "lib/parser.mly"
         ( ">" )
# 853 "lib/parser.ml"
     : (string))

let _menhir_action_51 =
  fun () ->
    (
# 106 "lib/parser.mly"
         ( "<=" )
# 861 "lib/parser.ml"
     : (string))

let _menhir_action_52 =
  fun () ->
    (
# 107 "lib/parser.mly"
         ( ">=" )
# 869 "lib/parser.ml"
     : (string))

let _menhir_action_53 =
  fun () ->
    (
# 108 "lib/parser.mly"
         ( "==" )
# 877 "lib/parser.ml"
     : (string))

let _menhir_action_54 =
  fun () ->
    (
# 109 "lib/parser.mly"
         ( "!=" )
# 885 "lib/parser.ml"
     : (string))

let _menhir_action_55 =
  fun _1 ->
    (
# 128 "lib/parser.mly"
                                           ( Plain _1 )
# 893 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_56 =
  fun _2 _4 ->
    (
# 129 "lib/parser.mly"
                                           ( Named(_2, _4) )
# 901 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_57 =
  fun _2 _4 _6 ->
    (
# 130 "lib/parser.mly"
                                           ( Refined(_2, _4, _6) )
# 909 "lib/parser.ml"
     : (Expr.s_expr Expr.refined_ty))

let _menhir_action_58 =
  fun _1 ->
    (
# 91 "lib/parser.mly"
                                                     ( SVar _1 )
# 917 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_59 =
  fun _1 ->
    (
# 92 "lib/parser.mly"
                                                     ( SInt _1 )
# 925 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_60 =
  fun () ->
    (
# 93 "lib/parser.mly"
                                                     ( SBool true )
# 933 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_61 =
  fun () ->
    (
# 94 "lib/parser.mly"
                                                     ( SBool false )
# 941 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_62 =
  fun _2 ->
    (
# 95 "lib/parser.mly"
                                                     ( _2 )
# 949 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_63 =
  fun _1 _3 ->
    (
# 96 "lib/parser.mly"
                                                     ( SCall(_1, _3) )
# 957 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_64 =
  fun _1 ->
    (
# 97 "lib/parser.mly"
                                                     ( SCall(_1, []) )
# 965 "lib/parser.ml"
     : (Expr.s_expr))

let _menhir_action_65 =
  fun _1 ->
    (
# 176 "lib/parser.mly"
                                                 ( TConst _1 )
# 973 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_66 =
  fun _1 _3 ->
    (
# 177 "lib/parser.mly"
                                                 ( TApp(_1, _3) )
# 981 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_67 =
  fun _2 ->
    (
# 178 "lib/parser.mly"
                                                 ( _2 )
# 989 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_68 =
  fun _1 ->
    (
# 170 "lib/parser.mly"
                                                             ( _1 )
# 997 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_69 =
  fun _3 _5 ->
    (
# 171 "lib/parser.mly"
                                                             (
				replace_ty_constants_with_vars _3 _5
			)
# 1007 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_70 =
  fun _1 ->
    (
# 141 "lib/parser.mly"
                                                   ( _1 )
# 1015 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_71 =
  fun _1 ->
    (
# 142 "lib/parser.mly"
                                                   ( _1 )
# 1023 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_72 =
  fun _3 _5 ->
    (
# 143 "lib/parser.mly"
                                          (
				replace_ty_constants_with_vars _3 _5
			)
# 1033 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_73 =
  fun _1 ->
    (
# 181 "lib/parser.mly"
                             ( [_1] )
# 1041 "lib/parser.ml"
     : (Expr.s_expr Expr.ty list))

let _menhir_action_74 =
  fun _1 _3 ->
    (
# 182 "lib/parser.mly"
                             ( _1 :: _3 )
# 1049 "lib/parser.ml"
     : (Expr.s_expr Expr.ty list))

let _menhir_action_75 =
  fun _1 ->
    (
# 58 "lib/parser.mly"
                   ( _1 )
# 1057 "lib/parser.ml"
     : (Expr.s_expr Expr.ty))

let _menhir_action_76 =
  fun _1 ->
    (
# 137 "lib/parser.mly"
                                             ( _1 )
# 1065 "lib/parser.ml"
     : (Expr.s_ty))

let _menhir_action_77 =
  fun _3 _5 ->
    (
# 138 "lib/parser.mly"
                                             ( replace_ty_constants_with_vars _3 _5 )
# 1073 "lib/parser.ml"
     : (Expr.s_ty))

let _menhir_action_78 =
  fun _1 ->
    (
# 61 "lib/parser.mly"
                   ( _1 )
# 1081 "lib/parser.ml"
     : (Expr.s_ty))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | ARROW ->
        "ARROW"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQUALS ->
        "EQUALS"
    | FALSE ->
        "FALSE"
    | FORALL ->
        "FORALL"
    | FUN ->
        "FUN"
    | GE ->
        "GE"
    | GT ->
        "GT"
    | IDENT _ ->
        "IDENT"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | LBRACKET ->
        "LBRACKET"
    | LE ->
        "LE"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | NE ->
        "NE"
    | NOT ->
        "NOT"
    | OR ->
        "OR"
    | PERCENT ->
        "PERCENT"
    | PLUS ->
        "PLUS"
    | RBRACKET ->
        "RBRACKET"
    | REC ->
        "REC"
    | RPAREN ->
        "RPAREN"
    | SLASH ->
        "SLASH"
    | SOME ->
        "SOME"
    | STAR ->
        "STAR"
    | THEN ->
        "THEN"
    | TRUE ->
        "TRUE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_ty_forall : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ty_forall_eof =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_78 _1 in
          MenhirBox_ty_forall_eof _v
      | _ ->
          _eRR ()
  
  let _menhir_run_172 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ty_forall_eof =
    fun _menhir_stack _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_76 _1 in
      _menhir_goto_ty_forall _menhir_stack _v _tok
  
  let _menhir_run_168 : type  ttv_stack. ((ttv_stack, _menhir_box_ty_forall_eof) _menhir_cell1_FORALL, _menhir_box_ty_forall_eof) _menhir_cell1_ident_list -> _ -> _ -> _menhir_box_ty_forall_eof =
    fun _menhir_stack _v _tok ->
      let MenhirCell1_ident_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_FORALL (_menhir_stack, _) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_77 _3 _5 in
      _menhir_goto_ty_forall _menhir_stack _v _tok
  
  let _menhir_run_161 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ty_eof =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_75 _1 in
          MenhirBox_ty_eof _v
      | _ ->
          _eRR ()
  
  let _menhir_run_157 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_eof =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_22 _1 in
          MenhirBox_expr_eof _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_001 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_60 () in
      _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_simple_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState003 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState002 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState147 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_154 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | ELSE | EOF | EQ | GE | GT | IN | LE | LT | MINUS | NE | OR | PERCENT | PLUS | RPAREN | SLASH | STAR | THEN ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_04 _2 in
          _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_simple_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _v = _menhir_action_64 _1 in
          _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NOT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | LET ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048
      | IF ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048
      | FUN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState002 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState003 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState004 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUALS ->
              let _menhir_s = MenhirState007 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FALSE ->
                  _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_59 _1 in
      _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_009 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState009 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_58 _1 in
      _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState012 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RPAREN ->
              let _menhir_stack = MenhirCell1_RPAREN (_menhir_stack, _menhir_s) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COLON ->
                  let _menhir_s = MenhirState014 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | SOME ->
                      _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAREN ->
                      _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | ARROW ->
                  let _menhir_s = MenhirState123 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TRUE ->
                      _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NOT ->
                      _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MINUS ->
                      _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAREN ->
                      _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LET ->
                      _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IF ->
                      _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FUN ->
                      _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FALSE ->
                      _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | IDENT _v ->
              _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ARROW ->
              let _menhir_s = MenhirState142 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FALSE ->
                  _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SOME (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_s = MenhirState016 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_017 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState017
      | RBRACKET ->
          let _1 = _v in
          let _v = _menhir_action_34 _1 in
          _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_ident_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState165 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState023 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState016 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_166 : type  ttv_stack. ((ttv_stack, _menhir_box_ty_forall_eof) _menhir_cell1_FORALL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ty_forall_eof) _menhir_state -> _menhir_box_ty_forall_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState167 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_022 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SOME (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_s = MenhirState023 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState026 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState028 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState043 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ARROW | COMMA | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_65 _1 in
          _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState031 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW | COMMA | ELSE | EOF | IF | IN | RBRACKET | RPAREN | THEN ->
          let _1 = _v in
          let _v = _menhir_action_65 _1 in
          _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_simple_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState132 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState167 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_119 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_68 _1 in
      _menhir_goto_some_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_some_simple_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_55 _1 in
      _menhir_goto_return_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_return_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState132 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_133 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_param_list as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_return_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState134 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_61 () in
      _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_120 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_RPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_return_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState121 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_109 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_SOME, ttv_result) _menhir_cell1_ident_list -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_SOME (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_69 _3 _5 in
      _menhir_goto_some_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_stack = MenhirCell1_simple_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState039 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COMMA | ELSE | EOF | IF | IN | RBRACKET | RPAREN | THEN ->
          let _1 = _v in
          let _v = _menhir_action_70 _1 in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_172 _menhir_stack _v _tok
      | MenhirState167 ->
          _menhir_run_168 _menhir_stack _v _tok
      | MenhirState159 ->
          _menhir_run_161 _menhir_stack _v _tok
      | MenhirState126 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState102 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_127 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IF ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState128 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | RPAREN ->
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_37 _1 _3 in
          _menhir_goto_param _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_param : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_param (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState139 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_39 _1 in
          _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState126 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COMMA | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_36 _1 in
          _menhir_goto_param _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_param_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState139 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState012 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_140 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_param -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_param (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_40 _1 _3 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_130 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState132 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ARROW ->
          let _menhir_s = MenhirState136 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState110, _v) in
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer
          | COLON ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState110, _v) in
              let _menhir_s = MenhirState112 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SOME ->
                  _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | ARROW | RPAREN ->
              let _v =
                let _1 = _v in
                _menhir_action_65 _1
              in
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState110 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_56 _2 _4 in
          _menhir_goto_return_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | IF ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState115 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_ty -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ty (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_67 _2 in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_107 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_SOME, ttv_result) _menhir_cell1_ident_list -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_SOME (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_72 _3 _5 in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_095 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_param_ty as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_41 _1 in
      _menhir_goto_param_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState102 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_101 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_param_ty as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_param_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState102 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_43 _1 in
          _menhir_goto_param_ty_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_param_ty_list : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_param_ty as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState102 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState094 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_param_ty, ttv_result) _menhir_cell1_param_ty -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_param_ty (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_44 _1 _3 in
      _menhir_goto_param_ty_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_097 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_param_ty as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_param_ty_list (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState099 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_093 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_param_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState094 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_087 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _1 = _v in
          let _v = _menhir_action_41 _1 in
          _menhir_goto_param_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_simple_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IF ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState084 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
          let MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_15 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_157 _menhir_stack _v _tok
      | MenhirState004 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState150 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState007 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState147 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState128 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState121 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState115 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState045 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState079 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_152 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_62 _2 in
          _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET _menhir_cell0_IDENT, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_17 _2 _4 _6 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_149 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          let _menhir_s = MenhirState150 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_148 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_19 _2 _4 _6 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_146 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState147 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_144 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState145 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_FUN _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_23 _2 _4 in
      _menhir_goto_fun_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_fun_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_18 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_137 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_param_list -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_param_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_24 _3 _6 in
      _menhir_goto_fun_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_135 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_param_list, ttv_result) _menhir_cell1_return_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_return_ty (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_param_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_26 _3 _6 _8 in
      _menhir_goto_fun_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_129 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_38 _1 _3 _5 in
      _menhir_goto_param _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_124 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_RPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_RPAREN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_25 _5 in
      _menhir_goto_fun_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_122 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_RPAREN, ttv_result) _menhir_cell1_return_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_return_ty (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_RPAREN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_27 _5 _7 in
      _menhir_goto_fun_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_116 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty (_menhir_stack, _, _4) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _6 = _v in
          let _v = _menhir_action_57 _2 _4 _6 in
          _menhir_goto_return_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_46 _1 _3 _5 in
      _menhir_goto_refined_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_refined_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState026 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_105 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _menhir_stack = MenhirCell1_refined_ty (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ARROW ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | COMMA ->
          let _1 = _v in
          let _v = _menhir_action_42 _1 in
          _menhir_goto_param_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_091 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_refined_ty -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState091 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_param_ty as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_42 _1 in
      _menhir_goto_param_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_089 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ARROW ->
              let _menhir_stack = MenhirCell1_refined_ty (_menhir_stack, _menhir_s, _v) in
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer
          | COMMA | ELSE | EOF | IF | IN | RBRACKET | RPAREN | THEN ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let _2 = _v in
              let _v = _menhir_action_29 _2 in
              _menhir_goto_function_ret_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | COMMA ->
          let _1 = _v in
          let _v = _menhir_action_42 _1 in
          _menhir_goto_param_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_function_ret_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState028 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState039 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_104 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_RPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_RPAREN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_30 _4 in
      _menhir_goto_function_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_function_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_71 _1 in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_100 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_param_ty, ttv_result) _menhir_cell1_param_ty_list -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_param_ty_list (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_param_ty (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_33 _2 _4 _7 in
      _menhir_goto_function_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_092 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_refined_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_refined_ty (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_32 _2 _5 in
      _menhir_goto_function_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_042 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_simple_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_ty (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_31 _1 _3 in
      _menhir_goto_function_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_085 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_simple_expr, ttv_result) _menhir_cell1_ty -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_16 _1 _3 _5 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_078 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState079 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_20 _1 in
          _menhir_goto_expr_comma_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_comma_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState048 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_080 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_21 _1 _3 in
      _menhir_goto_expr_comma_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_076 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_simple_expr -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_63 _1 _3 in
      _menhir_goto_simple_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_044 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IF ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState045 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | RPAREN ->
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_45 _1 _3 in
          _menhir_goto_refined_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_28 _1 in
      _menhir_goto_function_ret_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_035 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_ty (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState036 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACKET ->
          let _1 = _v in
          let _v = _menhir_action_73 _1 in
          _menhir_goto_ty_comma_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty_comma_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState036 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState031 ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_037 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_ty (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_74 _1 _3 in
      _menhir_goto_ty_comma_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_033 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_66 _1 _3 in
      _menhir_goto_simple_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_024 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_SOME as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState025 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_SOME as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState020 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState021 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_35 _1 _2 in
      _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_arithmetic_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState071 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState055 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState002 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState147 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_072 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr _menhir_cell0_relation_op as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | ELSE | EOF | IN | OR | RPAREN | THEN ->
          let MenhirCell0_relation_op (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_48 _1 _2 _3 in
          _menhir_goto_relation_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState055 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState057 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState059 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState061 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState064 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_relation_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState002 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState073 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState051 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState147 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_155 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_11 _2 in
      _menhir_goto_boolean_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_boolean_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_14 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_074 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_relation_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relation_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_12 _1 _3 in
      _menhir_goto_boolean_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_relation_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relation_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_13 _1 _3 in
      _menhir_goto_boolean_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_relation_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState051 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_relation_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState073 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
          let _1 = _v in
          let _v = _menhir_action_10 _1 in
          _menhir_goto_boolean_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_065 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | ELSE | EOF | EQ | GE | GT | IN | LE | LT | MINUS | NE | OR | PLUS | RPAREN | THEN ->
          let MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_06 _1 _3 in
          _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_062 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_09 _1 _3 in
      _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_060 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | ELSE | EOF | EQ | GE | GT | IN | LE | LT | MINUS | NE | OR | PLUS | RPAREN | THEN ->
          let MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_05 _1 _3 in
          _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_058 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_08 _1 _3 in
      _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_056 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_07 _1 _3 in
      _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_54 () in
          _menhir_goto_relation_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MINUS ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_49 () in
          _menhir_goto_relation_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LE ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_51 () in
          _menhir_goto_relation_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | GT ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_50 () in
          _menhir_goto_relation_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | GE ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_52 () in
          _menhir_goto_relation_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQ ->
          let _menhir_stack = MenhirCell1_arithmetic_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_53 () in
          _menhir_goto_relation_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | AND | COMMA | ELSE | EOF | IN | OR | RPAREN | THEN ->
          let _1 = _v in
          let _v = _menhir_action_47 _1 in
          _menhir_goto_relation_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_relation_op : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_arithmetic_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_relation_op (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | INT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState071
      | IDENT _v_1 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState071
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COMMA | ELSE | EOF | EQ | GE | GT | IN | LE | LT | MINUS | NE | OR | PERCENT | PLUS | RPAREN | SLASH | STAR | THEN ->
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON ->
          let _menhir_stack = MenhirCell1_simple_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState082 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AND | COMMA | ELSE | EOF | EQ | GE | GT | IN | LE | LT | MINUS | NE | OR | PERCENT | PLUS | RPAREN | SLASH | STAR | THEN ->
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_goto_arithmetic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_159 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ty_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState159 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_163 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ty_forall_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState163 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SOME ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FORALL ->
          let _menhir_stack = MenhirCell1_FORALL (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_s = MenhirState165 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let ty_forall_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_ty_forall_eof v = _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let ty_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_ty_eof v = _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let expr_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_expr_eof v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
