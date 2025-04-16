(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

type typed_program = typed_declaration list

and typed_declaration =
  | TDComment of span * string
  | TDValueBinding of span * typed_binding
  | TDTypeDefinition of span * typed_type_definition
  | TDFunctionDefinition of span * typed_function_definition
  | TDADTDefinition of span * typed_adt_definition

and typed_type_definition = {id: uid; typing: typing}

and typed_function_definition =
  { id: lid
  ; parameters: typed_parameter list
  ; body: typed_expression
  ; typing: typing }

and typed_adt_definition =
  {id: uid; polymorphics: lid list; variants: typed_variant list; typing: typing}

and typed_binding = {span: span; id: lid; body: typed_expression; typing: typing}

and typed_parameter =
  | TALID of span * typing * lid
  | TATuple of span * typing * typed_parameter list

and typed_expression =
  | TEInt of span * int
  | TEFloat of span * float
  | TEBool of span * bool
  | TEString of span * string
  | TEUnit of span
  | TEConstructor of span * typed_constructor
  | TELID of span * typing * lid
  | TETuple of span * typing * typed_expression list
  | TEList of span * typing * typed_expression list
  | TEBinaryOperation of span * typed_binary_operation
  | TEUnaryOperation of span * typed_unary_operation
  | TELet of span * typed_let_expr
  | TEIf of span * typed_if_expr
  | TEMatch of span * typed_match_expr
  | TELambda of span * typed_lambda_expr
  | TEApplication of span * typed_application
  | TEExpression of span * typing * typed_expression

and typed_constructor = {id: uid; body: typed_expression option; typing: typing}

and typed_binary_operation =
  { l: typed_expression
  ; operator: binary_operator
  ; r: typed_expression
  ; typing: typing }

and typed_unary_operation =
  {operator: unary_operator; body: typed_expression; typing: typing}

and typed_let_expr =
  {bindings: typed_binding list; body: typed_expression; typing: typing}

and typed_if_expr =
  { predicate: typed_expression
  ; truthy: typed_expression
  ; falsy: typed_expression
  ; typing: typing }

and typed_match_expr =
  {body: typed_expression; cases: typed_case list; typing: typing}

and typed_lambda_expr =
  {parameters: typed_parameter list; body: typed_expression; typing: typing}

and typed_application =
  {body: typed_expression; argument: typed_expression; typing: typing}

and typed_variant = {span: span; id: uid; typing: typing}

and typed_case =
  { span: span
  ; pattern: typed_pattern
  ; guard: typed_expression option
  ; body: typed_expression
  ; typing: typing }

and typed_pattern =
  | TPInt of span * int
  | TPFloat of span * float
  | TPBool of span * bool
  | TPString of span * string
  | TPLID of span * typing * lid
  | TPTuple of span * typing * typed_pattern list
  | TPList of span * typing * typed_pattern list
  | TPListSpread of span * typing * typed_pattern list
  | TPConstructor of span * typed_constructor_pattern
  | TPOr of span * typed_or_pattern

and typed_constructor_pattern =
  {id: uid; pattern: typed_pattern option; typing: typing}

and typed_or_pattern = {l: typed_pattern; r: typed_pattern; typing: typing}
[@@deriving show {with_path= false}]
