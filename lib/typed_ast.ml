(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type typed_program = typed_declaration list

and typed_declaration =
  | TDComment of Ast.span * string
  | TDValueBinding of Ast.span * typed_binding
  | TDTypeDefinition of Ast.span * typed_type_definition
  | TDFunctionDefinition of Ast.span * typed_function_definition
  | TDADTDefinition of Ast.span * typed_adt_definition

and typed_type_definition = {id: Ast.uid; typing: Ast.typing}

and typed_function_definition =
  { id: Ast.lid
  ; parameters: typed_parameter list
  ; body: typed_expression
  ; typing: Ast.typing }

and typed_adt_definition =
  { id: Ast.uid
  ; polymorphics: Ast.lid list
  ; variants: typed_variant list
  ; typing: Ast.typing }

and typed_binding =
  {span: Ast.span; id: Ast.lid; body: typed_expression; typing: Ast.typing}

and typed_parameter =
  | TALID of Ast.span * Ast.typing * Ast.lid
  | TATuple of Ast.span * Ast.typing * typed_parameter list

and typed_expression =
  | TEInt of Ast.span * int
  | TEFloat of Ast.span * float
  | TEBool of Ast.span * bool
  | TEString of Ast.span * string
  | TEUnit of Ast.span
  | TEConstructor of Ast.span * typed_constructor
  | TELID of Ast.span * Ast.typing * Ast.lid
  | TETuple of Ast.span * Ast.typing * typed_expression list
  | TEList of Ast.span * Ast.typing * typed_expression list
  | TEBinaryOperation of Ast.span * typed_binary_operation
  | TEUnaryOperation of Ast.span * typed_unary_operation
  | TELet of Ast.span * typed_let_expr
  | TEIf of Ast.span * typed_if_expr
  | TEMatch of Ast.span * typed_match_expr
  | TELambda of Ast.span * typed_lambda_expr
  | TEApplication of Ast.span * typed_application
  | TEExpression of Ast.span * Ast.typing * typed_expression

and typed_constructor =
  {id: Ast.uid; body: typed_expression option; typing: Ast.typing}

and typed_binary_operation =
  { l: typed_expression
  ; operator: Ast.binary_operator
  ; r: typed_expression
  ; typing: Ast.typing }

and typed_unary_operation =
  {operator: Ast.unary_operator; body: typed_expression; typing: Ast.typing}

and typed_let_expr =
  {bindings: typed_binding list; body: typed_expression; typing: Ast.typing}

and typed_if_expr =
  { predicate: typed_expression
  ; truthy: typed_expression
  ; falsy: typed_expression
  ; typing: Ast.typing }

and typed_match_expr =
  {body: typed_expression; cases: typed_case list; typing: Ast.typing}

and typed_lambda_expr =
  {parameters: typed_parameter list; body: typed_expression; typing: Ast.typing}

and typed_application =
  {body: typed_expression; argument: typed_expression; typing: Ast.typing}

and typed_variant = {span: Ast.span; id: Ast.uid; typing: Ast.typing}

and typed_case =
  { span: Ast.span
  ; pattern: typed_pattern
  ; guard: typed_expression option
  ; body: typed_expression
  ; typing: Ast.typing }

and typed_pattern =
  | TPInt of Ast.span * int
  | TPFloat of Ast.span * float
  | TPBool of Ast.span * bool
  | TPString of Ast.span * string
  | TPLID of Ast.span * Ast.typing * Ast.lid
  | TPTuple of Ast.span * Ast.typing * typed_pattern list
  | TPList of Ast.span * Ast.typing * typed_pattern list
  | TPListSpread of Ast.span * Ast.typing * typed_pattern list
  | TPConstructor of Ast.span * typed_constructor_pattern
  | TPOr of Ast.span * typed_or_pattern

and typed_constructor_pattern =
  {id: Ast.uid; pattern: typed_pattern option; typing: Ast.typing}

and typed_or_pattern = {l: typed_pattern; r: typed_pattern; typing: Ast.typing}
[@@deriving show {with_path= false}]
