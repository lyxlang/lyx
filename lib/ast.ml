(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type position = Lexing.position =
  {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}

and span = position * position

and uid = string

and lid = string

and program = declaration list

and declaration =
  | DComment of span * string
  | DValueBinding of span * binding
  | DTypeDefinition of span * type_definition
  | DFunctionDefinition of span * function_definition
  | DADTDefinition of span * adt_definition

and type_definition = {id: uid; body: typing}

and function_definition =
  {id: lid; parameters: parameter list; signature: signature; body: expression}

and adt_definition = {id: uid; polymorphics: lid list; variants: variant list}

and binding = {span: span; id: lid; signature: signature; body: expression}

and signature = typing option

and parameter = ALID of span * lid | ATuple of span * parameter list

and typing =
  | TInt of span
  | TFloat of span
  | TBool of span
  | TString of span
  | TUnit of span
  | TConstructor of span * typing_constructor
  | TPolymorphic of span * lid
  | TTuple of span * typing list
  | TList of span * typing
  | TFunction of span * function_typing

and typing_constructor = {id: uid; typing: typing option}

and function_typing = {l: typing; r: typing}

and expression =
  | EInt of span * int
  | EFloat of span * float
  | EBool of span * bool
  | EString of span * string
  | EUnit of span
  | EConstructor of span * constructor
  | ELID of span * lid
  | ETuple of span * expression list
  | EList of span * expression list
  | EBinaryOperation of span * binary_operation
  | EUnaryOperation of span * unary_operation
  | ELet of span * let_expr
  | EIf of span * if_expr
  | EMatch of span * match_expr
  | ELambda of span * lambda_expr
  | EApplication of span * application
  | EExpression of span * expression_with_signature

and constructor = {id: uid; body: expression option}

and binary_operation = {l: expression; operator: binary_operator; r: expression}

and unary_operation = {operator: unary_operator; body: expression}

and let_expr = {bindings: binding list; body: expression}

and if_expr = {predicate: expression; truthy: expression; falsy: expression}

and match_expr = {body: expression; cases: case list}

and lambda_expr = {parameters: parameter list; body: expression}

and application = {body: expression; argument: expression}

and expression_with_signature = {body: expression; signature: signature}

and binary_operator =
  | BPipe
  | BOr
  | BAnd
  | BEqual
  | BNotEqual
  | BGreaterThan
  | BGreaterOrEqual
  | BLessThan
  | BLessOrEqual
  | BConcatenate
  | BAdd
  | BSubstract
  | BMultiply
  | BDivide
  | BModulo
  | BExponentiate

and unary_operator = UPlus | UMinus | UNot

and variant = {span: span; id: uid; typing: typing option}

and case =
  {span: span; pattern: pattern; guard: expression option; body: expression}

and pattern =
  | PInt of span * int
  | PFloat of span * float
  | PBool of span * bool
  | PString of span * string
  | PLID of span * lid
  | PTuple of span * pattern list
  | PList of span * pattern list
  | PListSpread of span * pattern list
  | PConstructor of span * constructor_pattern
  | POr of span * or_pattern

and constructor_pattern = {id: uid; pattern: pattern option}

and or_pattern = {l: pattern; r: pattern} [@@deriving show {with_path= false}]
