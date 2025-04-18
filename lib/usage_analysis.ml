(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

module Env = struct
  type t =
    { values: (string, span) Hashtbl.t
    ; types: (string, span) Hashtbl.t
    ; variants: (uid, span) Hashtbl.t }

  let scope {values; types; variants} =
    { values= Hashtbl.copy values
    ; types= Hashtbl.copy types
    ; variants= Hashtbl.copy variants }

  let add_value env = Hashtbl.replace env.values

  let add_type env = Hashtbl.replace env.types

  let add_variant (env : t) = Hashtbl.replace env.variants

  let value_exists env = Hashtbl.mem env.values

  let type_exists env name =
    Hashtbl.mem env.types name
    || List.mem name ["Int"; "Float"; "String"; "Bool"; "Unit"]

  let variant_exists (env : t) = Hashtbl.mem env.variants

  let root =
    let env =
      { values= Hashtbl.create 100
      ; types= Hashtbl.create 100
      ; variants= Hashtbl.create 100 }
    in
    List.iter
      (fun name -> add_value env name (Lexing.dummy_pos, Lexing.dummy_pos))
      [ "sqrt"
      ; "\u{03C0}"
      ; "map"
      ; "filter"
      ; "foldLeft"
      ; "foldRight"
      ; "printString"
      ; "printNumber" ] ;
    env
end

module Output = struct
  type error = Undefined of {name: string; span: span}

  exception Analysis_error of error list

  let errors : error list ref = ref []

  let add_error error = errors := !errors @ [error]

  let code_of_error = function Undefined _ -> 1001

  let string_of_error = function
    | Undefined {name; _} ->
        Printf.sprintf "Undefined identifier: \u{201C}%s\u{201D}." name

  let span_of_error = function Undefined {span; _} -> span

  let raise_errors () = if !errors <> [] then raise (Analysis_error !errors)
end

let rec analyze_program program =
  List.iter (analyze_declaration Env.root) program ;
  Output.raise_errors () ;
  program

and analyze_declaration env = function
  | DComment _ ->
      ()
  | DValueBinding (span, {id; signature; body; _}) ->
      Env.add_value env id span ;
      Option.iter (analyze_typing env) signature ;
      analyze_expression env body
  | DTypeDefinition (span, {id; body}) ->
      Env.add_type env id span ; analyze_typing env body
  | DADTDefinition (span, {id; polymorphics; variants}) ->
      Env.add_type env id span ;
      let adt_env = Env.scope env in
      List.iter (fun p -> Env.add_type adt_env p span) polymorphics ;
      List.iter
        (fun ({span; id; typing} : variant) ->
          Env.add_variant env id span ;
          Option.iter (analyze_typing adt_env ~inADT:true) typing )
        variants
  | DFunctionDefinition _ ->
      failwith "Function definitions should be desugared before analysis."

and analyze_typing ?(inADT = false) env = function
  | TInt _ | TFloat _ | TBool _ | TString _ | TUnit _ ->
      ()
  | TConstructor (span, {id; typing}) ->
      if not (Env.type_exists env id || Env.variant_exists env id) then
        Output.add_error (Output.Undefined {name= id; span}) ;
      Option.iter (analyze_typing env) typing
  | TPolymorphic (span, id) ->
      if inADT && not (Env.type_exists env id) then
        Output.add_error (Output.Undefined {name= id; span})
  | TTuple (_, typings) ->
      List.iter (analyze_typing env) typings
  | TList (_, typing) ->
      analyze_typing env typing
  | TFunction (_, {l; r}) ->
      analyze_typing env l ; analyze_typing env r

and analyze_expression env = function
  | EInt _ | EFloat _ | EBool _ | EString _ | EUnit _ ->
      ()
  | EConstructor (span, {id; body}) ->
      if not (Env.type_exists env id || Env.variant_exists env id) then
        Output.add_error (Output.Undefined {name= id; span}) ;
      Option.iter (analyze_expression env) body
  | ELID (span, id) ->
      if not (Env.value_exists env id) then
        Output.add_error (Output.Undefined {name= id; span})
  | ETuple (_, expressions) | EList (_, expressions) ->
      List.iter (analyze_expression env) expressions
  | EBinaryOperation (_, {l; r; _}) ->
      analyze_expression env l ; analyze_expression env r
  | EUnaryOperation (_, {body; _}) ->
      analyze_expression env body
  | ELet _ ->
      failwith "Let expressions should be desugared before analysis."
  | EDesugaredLet (_, {binding; body}) ->
      let {span; id; signature; body= body'} = binding in
      let let_env = Env.scope env in
      Env.add_value let_env id span ;
      Option.iter (analyze_typing env) signature ;
      analyze_expression env body' ;
      analyze_expression let_env body
  | EIf (_, {predicate; truthy; falsy}) ->
      analyze_expression env predicate ;
      analyze_expression env truthy ;
      analyze_expression env falsy
  | EMatch (_, {body; cases}) ->
      analyze_expression env body ;
      List.iter (analyze_case env) cases
  | ELambda _ ->
      failwith "Lambda expressions should be desugared before analysis."
  | EDesugaredLambda (_, {parameter; body}) ->
      let lambda_env = Env.scope env in
      analyze_parameter lambda_env parameter ;
      analyze_expression lambda_env body
  | EApplication (_, {body; argument}) ->
      analyze_expression env body ;
      analyze_expression env argument
  | EExpression (_, {body; signature}) ->
      analyze_expression env body ;
      Option.iter (analyze_typing env) signature

and analyze_case env {pattern; guard; body; _} =
  let case_env = Env.scope env in
  analyze_pattern case_env pattern ;
  Option.iter (analyze_expression case_env) guard ;
  analyze_expression case_env body

and analyze_pattern env = function
  | PInt _ | PFloat _ | PBool _ | PString _ ->
      ()
  | PLID (span, id) ->
      Env.add_value env id span
  | PTuple (_, patterns) | PList (_, patterns) | PListSpread (_, patterns) ->
      List.iter (analyze_pattern env) patterns
  | PConstructor (span, {id; pattern}) ->
      if not (Env.type_exists env id || Env.variant_exists env id) then
        Output.add_error (Output.Undefined {name= id; span}) ;
      Option.iter (analyze_pattern env) pattern
  | POr (_, {l; r}) ->
      analyze_pattern env l ; analyze_pattern env r

and analyze_parameter env = function
  | ALID (span, id) ->
      Env.add_value env id span
  | ATuple (_, parameters) ->
      List.iter (analyze_parameter env) parameters
