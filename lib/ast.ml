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
[@@deriving show {with_path= false}]
