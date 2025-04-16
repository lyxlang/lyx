(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

let rec curry_lambda span parameters body =
  match parameters with
  | [] ->
      body
  | p :: tail ->
      EDesugaredLambda (span, {parameter= p; body= curry_lambda span tail body})

let rec desugar_program program = List.map desugar_declaration program

and desugar_declaration decl =
  match decl with
  | DComment _ | DTypeDefinition _ | DADTDefinition _ ->
      decl
  | DValueBinding (span, binding) ->
      DValueBinding (span, desugar_binding binding)
  | DFunctionDefinition (span, func_def) ->
      DFunctionDefinition (span, desugar_function_definition func_def)

and desugar_binding binding =
  {binding with body= desugar_expression binding.body}

and desugar_function_definition (func_def : function_definition) =
  {func_def with body= desugar_expression func_def.body}

and desugar_expression expr =
  match expr with
  | EInt _ | EFloat _ | EBool _ | EString _ | EUnit _ | ELID _ ->
      expr
  | EConstructor (span, {id; body}) ->
      EConstructor (span, {id; body= Option.map desugar_expression body})
  | ETuple (span, exprs) ->
      ETuple (span, List.map desugar_expression exprs)
  | EList (span, exprs) ->
      EList (span, List.map desugar_expression exprs)
  | EBinaryOperation (span, {l; operator; r}) ->
      EBinaryOperation
        (span, {l= desugar_expression l; operator; r= desugar_expression r})
  | EUnaryOperation (span, {operator; body}) ->
      EUnaryOperation (span, {operator; body= desugar_expression body})
  | ELet (span, {bindings; body}) ->
      ELet
        ( span
        , { bindings= List.map desugar_binding bindings
          ; body= desugar_expression body } )
  | EIf (span, {predicate; truthy; falsy}) ->
      EIf
        ( span
        , { predicate= desugar_expression predicate
          ; truthy= desugar_expression truthy
          ; falsy= desugar_expression falsy } )
  | EMatch (span, {body; cases}) ->
      EMatch
        ( span
        , {body= desugar_expression body; cases= List.map desugar_case cases} )
  | ELambda (span, {parameters; body}) ->
      curry_lambda span parameters (desugar_expression body)
  | EDesugaredLambda (span, {parameter; body}) ->
      EDesugaredLambda (span, {parameter; body= desugar_expression body})
  | EApplication (span, {body; argument}) ->
      EApplication
        ( span
        , {body= desugar_expression body; argument= desugar_expression argument}
        )
  | EExpression (span, {body; signature}) ->
      EExpression (span, {body= desugar_expression body; signature})

and desugar_case case =
  { case with
    guard= Option.map desugar_expression case.guard
  ; body= desugar_expression case.body }
