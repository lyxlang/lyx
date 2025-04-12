(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type span = {start: int; fin: int}

and 'a located = {loc: span; value: 'a}

and program = decl located list

and decl =
  | Decl of tlbind
  | Decls of tlbind list
  | DeclADT of
      { id: string located
      ; polys: string located list
      ; variants: variant located list }
  | DeclAlias of {id: string located; typing: typing located}
  | Comment of string

and olid = Wildcard | L of string

and param = PRLID of string located | PRTuple of tuple_param

and ann = typing located option

and tuple_param = param located list

and expr =
  | EParenthesized of expr located
  | ETyped of {body: expr located; signature: ann}
  | EBoolOp of {l: expr located; op: bool_op located; r: expr located}
  | ECompOp of {l: expr located; op: comp_op located; r: expr located}
  | EPipeOp of {l: expr located; r: expr located}
  | EConcatOp of {l: expr located; r: expr located}
  | EAddOp of {l: expr located; op: add_op located; r: expr located}
  | EMulOp of {l: expr located; op: mul_op located; r: expr located}
  | EUnOp of {op: un_op located; body: expr located}
  | EExpOp of {l: expr located; r: expr located}
  | EApp of {fn: expr located; arg: expr located}
  | ELambda of {params: param located list; body: expr located}
  | EMatch of {ref: expr located; cases: case located list}
  | ELets of {binds: bind located list; body: expr located}
  | ELet of {bind: bind located; body: expr located}
  | EIf of {predicate: expr located; truthy: expr located; falsy: expr located}
  | EUID of string located
  | ELID of string located
  | ETuple of expr located list
  | EList of expr located list
  | EUnit
  | EBool of bool located
  | EString of string located
  | EFloat of string located
  | EInt of string located

and bool_op = OpBoolAnd | OpBoolOr

and comp_op = OpGt | OpGeq | OpLt | OpLeq | OpEq | OpFeq | OpNeq | OpNFeq

and add_op = OpAdd | OpSub

and mul_op = OpMul | OpDiv | OpMod

and un_op = UnPlus | UnNeg | UnBoolNot

and tlbind =
  { id: olid located
  ; params: param located list
  ; signature: ann
  ; body: expr located }

and bind =
  { id: string located
  ; params: param located list
  ; signature: ann
  ; body: expr located }

and case =
  | Case of {pat: pattern located; body: expr located}
  | CaseGuard of {pat: pattern located; guard: expr located; body: expr located}

and pattern =
  | PInt of string located
  | PFloat of string located
  | PString of string located
  | PBool of bool located
  | POLID of olid located
  | PTail of olid located
  | PConstructor of {id: string located; params: pattern located list}
  | PList of list_pat located
  | PListSpd of list_spd_pat located
  | PTuple of tuple_pat located
  | POr of {l: pattern located; r: pattern located}
  | PParenthesized of pattern located

and list_pat = pattern located list

and list_spd_pat = pattern located list

and tuple_pat = pattern located list

and variant = {id: string located; typing: typing located option}

and typing =
  | TInt
  | TFloat
  | TString
  | TBool
  | TUnit
  | TList of typing located
  | TTuple of typing located list
  | TFunc of {l: typing located; r: typing located}
  | TPoly of string located
  | TConstructor of variant
  | TTyping of typing located

val pp_span : Format.formatter -> span -> unit

val show_span : span -> string

val pp_located :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit

val show_located : (Format.formatter -> 'a -> unit) -> 'a located -> string

val pp_program : Format.formatter -> program -> unit

val show_program : program -> string

val pp_decl : Format.formatter -> decl -> unit

val show_decl : decl -> string

val pp_olid : Format.formatter -> olid -> unit

val show_olid : olid -> string

val pp_param : Format.formatter -> param -> unit

val show_param : param -> string

val pp_ann : Format.formatter -> ann -> unit

val show_ann : ann -> string

val pp_tuple_param : Format.formatter -> tuple_param -> unit

val show_tuple_param : tuple_param -> string

val pp_expr : Format.formatter -> expr -> unit

val show_expr : expr -> string

val pp_bool_op : Format.formatter -> bool_op -> unit

val show_bool_op : bool_op -> string

val pp_comp_op : Format.formatter -> comp_op -> unit

val show_comp_op : comp_op -> string

val pp_add_op : Format.formatter -> add_op -> unit

val show_add_op : add_op -> string

val pp_mul_op : Format.formatter -> mul_op -> unit

val show_mul_op : mul_op -> string

val pp_un_op : Format.formatter -> un_op -> unit

val show_un_op : un_op -> string

val pp_tlbind : Format.formatter -> tlbind -> unit

val show_tlbind : tlbind -> string

val pp_bind : Format.formatter -> bind -> unit

val show_bind : bind -> string

val pp_case : Format.formatter -> case -> unit

val show_case : case -> string

val pp_pattern : Format.formatter -> pattern -> unit

val show_pattern : pattern -> string

val pp_list_pat : Format.formatter -> list_pat -> unit

val show_list_pat : list_pat -> string

val pp_list_spd_pat : Format.formatter -> list_spd_pat -> unit

val show_list_spd_pat : list_spd_pat -> string

val pp_tuple_pat : Format.formatter -> tuple_pat -> unit

val show_tuple_pat : tuple_pat -> string

val pp_variant : Format.formatter -> variant -> unit

val show_variant : variant -> string

val pp_typing : Format.formatter -> typing -> unit

val show_typing : typing -> string
