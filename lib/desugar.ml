(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

let rec desugar_program program = List.map desugar_declaration program

and desugar_declaration decl =
  match decl with
  | DComment _ | DTypeDefinition _ | DADTDefinition _ ->
      decl
  | DValueBinding (span, binding) ->
      DValueBinding (span, desugar_binding binding)
  | DFunctionDefinition (span, f) ->
      transform_function_definition span f

and desugar_binding binding =
  {binding with body= desugar_expression binding.body}

and desugar_expression expr =
  match expr with
  | EInt _
  | EFloat _
  | EBool _
  | EString _
  | EUnit _
  | ELID _
  | EDesugaredLambda _
  | EDesugaredLet _ ->
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
      desugar_let span bindings body
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
      curry_lambda span parameters body
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

and desugar_let span bindings body =
  match bindings with
  | [] ->
      desugar_expression body
  | b :: tail ->
      EDesugaredLet
        (span, {binding= desugar_binding b; body= desugar_let span tail body})

and curry_lambda span parameters body =
  match parameters with
  | [] ->
      desugar_expression body
  | p :: tail ->
      EDesugaredLambda (span, {parameter= p; body= curry_lambda span tail body})

and transform_function_definition span {id; parameters; signature; body} =
  let binding =
    {span; id; signature; body= curry_lambda span parameters body}
  in
  DValueBinding (span, binding)
