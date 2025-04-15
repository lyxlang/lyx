(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type span = {start: int; fin: int}

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

and or_pattern = {l: pattern; r: pattern}

val pp_span : Format.formatter -> span -> unit

val show_span : span -> string

val pp_uid : Format.formatter -> uid -> unit

val show_uid : uid -> string

val pp_lid : Format.formatter -> lid -> unit

val show_lid : lid -> string

val pp_program : Format.formatter -> program -> unit

val show_program : program -> string

val pp_declaration : Format.formatter -> declaration -> unit

val show_declaration : declaration -> string

val pp_type_definition : Format.formatter -> type_definition -> unit

val show_type_definition : type_definition -> string

val pp_function_definition : Format.formatter -> function_definition -> unit

val show_function_definition : function_definition -> string

val pp_adt_definition : Format.formatter -> adt_definition -> unit

val show_adt_definition : adt_definition -> string

val pp_binding : Format.formatter -> binding -> unit

val show_binding : binding -> string

val pp_signature : Format.formatter -> signature -> unit

val show_signature : signature -> string

val pp_parameter : Format.formatter -> parameter -> unit

val show_parameter : parameter -> string

val pp_typing : Format.formatter -> typing -> unit

val show_typing : typing -> string

val pp_typing_constructor : Format.formatter -> typing_constructor -> unit

val show_typing_constructor : typing_constructor -> string

val pp_function_typing : Format.formatter -> function_typing -> unit

val show_function_typing : function_typing -> string

val pp_expression : Format.formatter -> expression -> unit

val show_expression : expression -> string

val pp_constructor : Format.formatter -> constructor -> unit

val show_constructor : constructor -> string

val pp_binary_operation : Format.formatter -> binary_operation -> unit

val show_binary_operation : binary_operation -> string

val pp_unary_operation : Format.formatter -> unary_operation -> unit

val show_unary_operation : unary_operation -> string

val pp_let_expr : Format.formatter -> let_expr -> unit

val show_let_expr : let_expr -> string

val pp_if_expr : Format.formatter -> if_expr -> unit

val show_if_expr : if_expr -> string

val pp_match_expr : Format.formatter -> match_expr -> unit

val show_match_expr : match_expr -> string

val pp_lambda_expr : Format.formatter -> lambda_expr -> unit

val show_lambda_expr : lambda_expr -> string

val pp_application : Format.formatter -> application -> unit

val show_application : application -> string

val pp_expression_with_signature :
  Format.formatter -> expression_with_signature -> unit

val show_expression_with_signature : expression_with_signature -> string

val pp_binary_operator : Format.formatter -> binary_operator -> unit

val show_binary_operator : binary_operator -> string

val pp_unary_operator : Format.formatter -> unary_operator -> unit

val show_unary_operator : unary_operator -> string

val pp_variant : Format.formatter -> variant -> unit

val show_variant : variant -> string

val pp_case : Format.formatter -> case -> unit

val show_case : case -> string

val pp_pattern : Format.formatter -> pattern -> unit

val show_pattern : pattern -> string

val pp_constructor_pattern : Format.formatter -> constructor_pattern -> unit

val show_constructor_pattern : constructor_pattern -> string

val pp_or_pattern : Format.formatter -> or_pattern -> unit

val show_or_pattern : or_pattern -> string
