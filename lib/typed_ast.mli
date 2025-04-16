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

and typed_type_definition = {id: string; typing: Ast.typing}

and typed_function_definition =
  { id: string
  ; parameters: typed_parameter list
  ; body: typed_expression
  ; typing: Ast.typing }

and typed_adt_definition =
  { id: string
  ; polymorphics: string list
  ; variants: typed_variant list
  ; typing: Ast.typing }

and typed_binding =
  {span: Ast.span; id: string; body: typed_expression; typing: Ast.typing}

and typed_parameter =
  | TALID of Ast.span * Ast.typing * string
  | TATuple of Ast.span * Ast.typing * typed_parameter list

and typed_expression =
  | TEInt of Ast.span * int
  | TEFloat of Ast.span * float
  | TEBool of Ast.span * bool
  | TEString of Ast.span * string
  | TEUnit of Ast.span
  | TEConstructor of Ast.span * typed_constructor
  | TELID of Ast.span * Ast.typing * string
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
  {id: string; body: typed_expression option; typing: Ast.typing}

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

and typed_variant = {span: Ast.span; id: string; typing: Ast.typing}

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
  | TPLID of Ast.span * Ast.typing * string
  | TPTuple of Ast.span * Ast.typing * typed_pattern list
  | TPList of Ast.span * Ast.typing * typed_pattern list
  | TPListSpread of Ast.span * Ast.typing * typed_pattern list
  | TPConstructor of Ast.span * typed_constructor_pattern
  | TPOr of Ast.span * typed_or_pattern

and typed_constructor_pattern =
  {id: string; pattern: typed_pattern option; typing: Ast.typing}

and typed_or_pattern = {l: typed_pattern; r: typed_pattern; typing: Ast.typing}

val pp_typed_program : Format.formatter -> typed_program -> unit

val show_typed_program : typed_program -> string

val pp_typed_declaration : Format.formatter -> typed_declaration -> unit

val show_typed_declaration : typed_declaration -> string

val pp_typed_type_definition : Format.formatter -> typed_type_definition -> unit

val show_typed_type_definition : typed_type_definition -> string

val pp_typed_function_definition :
  Format.formatter -> typed_function_definition -> unit

val show_typed_function_definition : typed_function_definition -> string

val pp_typed_adt_definition : Format.formatter -> typed_adt_definition -> unit

val show_typed_adt_definition : typed_adt_definition -> string

val pp_typed_binding : Format.formatter -> typed_binding -> unit

val show_typed_binding : typed_binding -> string

val pp_typed_parameter : Format.formatter -> typed_parameter -> unit

val show_typed_parameter : typed_parameter -> string

val pp_typed_expression : Format.formatter -> typed_expression -> unit

val show_typed_expression : typed_expression -> string

val pp_typed_constructor : Format.formatter -> typed_constructor -> unit

val show_typed_constructor : typed_constructor -> string

val pp_typed_binary_operation :
  Format.formatter -> typed_binary_operation -> unit

val show_typed_binary_operation : typed_binary_operation -> string

val pp_typed_unary_operation : Format.formatter -> typed_unary_operation -> unit

val show_typed_unary_operation : typed_unary_operation -> string

val pp_typed_let_expr : Format.formatter -> typed_let_expr -> unit

val show_typed_let_expr : typed_let_expr -> string

val pp_typed_if_expr : Format.formatter -> typed_if_expr -> unit

val show_typed_if_expr : typed_if_expr -> string

val pp_typed_match_expr : Format.formatter -> typed_match_expr -> unit

val show_typed_match_expr : typed_match_expr -> string

val pp_typed_lambda_expr : Format.formatter -> typed_lambda_expr -> unit

val show_typed_lambda_expr : typed_lambda_expr -> string

val pp_typed_application : Format.formatter -> typed_application -> unit

val show_typed_application : typed_application -> string

val pp_typed_variant : Format.formatter -> typed_variant -> unit

val show_typed_variant : typed_variant -> string

val pp_typed_case : Format.formatter -> typed_case -> unit

val show_typed_case : typed_case -> string

val pp_typed_pattern : Format.formatter -> typed_pattern -> unit

val show_typed_pattern : typed_pattern -> string

val pp_typed_constructor_pattern :
  Format.formatter -> typed_constructor_pattern -> unit

val show_typed_constructor_pattern : typed_constructor_pattern -> string

val pp_typed_or_pattern : Format.formatter -> typed_or_pattern -> unit

val show_typed_or_pattern : typed_or_pattern -> string
