(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

module Output = struct
  type error =
    | Undefined of {name: string; span: span}
    | Reserved of {name: string; span: span}
    | Duplicate of {name: string; span: span}

  type[@warning "-37"] warning = Unused of {name: string; span: span}

  type t = {mutable errors: error list; mutable warnings: warning list}

  exception Analysis_error of Reporter.report list

  let output = {errors= []; warnings= []}

  let add_error error = output.errors <- output.errors @ [error]

  let[@warning "-32"] add_warning warning =
    output.warnings <- output.warnings @ [warning]

  let code_of_error = function
    | Undefined _ ->
        1001
    | Reserved _ ->
        1002
    | Duplicate _ ->
        1003

  let string_of_error = function
    | Undefined {name; _} ->
        Printf.sprintf "Undefined identifier: \u{201C}%s\u{201D}." name
    | Reserved {name; _} ->
        Printf.sprintf "Reserved identifier: \u{201C}%s\u{201D}." name
    | Duplicate {name; _} ->
        Printf.sprintf "Duplicate identifier: \u{201C}%s\u{201D}." name

  let span_of_error = function
    | Undefined {span; _} ->
        span
    | Reserved {span; _} ->
        span
    | Duplicate {span; _} ->
        span

  let code_of_warning = function Unused _ -> 1001

  let string_of_warning = function
    | Unused {name; _} ->
        Printf.sprintf "Unused identifier: \u{201C}%s\u{201D}." name

  let span_of_warning = function Unused {span; _} -> span

  let raise_errors () =
    if output.errors <> [] then
      raise
        (Analysis_error
           (List.map
              (fun error ->
                Reporter.create_report Reporter.Error (code_of_error error)
                  (string_of_error error) (span_of_error error) )
              output.errors ) )

  let report_warnings json =
    Reporter.print_reports ~json
      (List.map
         (fun warning ->
           Reporter.create_report Reporter.Warning (code_of_warning warning)
             (string_of_warning warning)
             (span_of_warning warning) )
         output.warnings )
end

module Env = struct
  type t =
    { values: (string, span) Hashtbl.t
    ; types: (string, span) Hashtbl.t
    ; variants: (uid, span) Hashtbl.t }

  let scope {values; types; variants} =
    { values= Hashtbl.copy values
    ; types= Hashtbl.copy types
    ; variants= Hashtbl.copy variants }

  let std_types = ["Int"; "Float"; "String"; "Bool"; "Unit"]

  let std_lib =
    [ "sqrt"
    ; "\u{03C0}"
    ; "map"
    ; "filter"
    ; "foldLeft"
    ; "foldRight"
    ; "printString"
    ; "printNumber" ]

  let values env = env.values

  let value_exists env = Hashtbl.mem env.values

  let type_exists env name =
    Hashtbl.mem env.types name || List.mem name std_types

  let variant_exists (env : t) = Hashtbl.mem env.variants

  let add_value env name span =
    if List.mem name std_lib then Output.add_error (Output.Reserved {name; span})
    else if (not (String.starts_with ~prefix:"_" name)) && value_exists env name
    then Output.add_error (Output.Duplicate {name; span}) ;
    Hashtbl.replace env.values name span

  let add_type env name span =
    if List.mem name std_types then
      Output.add_error (Output.Reserved {name; span})
    else if type_exists env name || variant_exists env name then
      Output.add_error (Output.Duplicate {name; span}) ;
    Hashtbl.replace env.types name span

  let add_variant env name span =
    if List.mem name std_types then
      Output.add_error (Output.Reserved {name; span})
    else if variant_exists env name || type_exists env name then
      Output.add_error (Output.Duplicate {name; span}) ;
    Hashtbl.replace env.variants name span

  let root =
    let env =
      { values= Hashtbl.create 100
      ; types= Hashtbl.create 100
      ; variants= Hashtbl.create 100 }
    in
    List.iter
      (fun name ->
        Hashtbl.replace env.values name (Lexing.dummy_pos, Lexing.dummy_pos) )
      std_lib ;
    env
end

let rec analyze_program ?(json = false) program =
  List.iter (hoist_declaration Env.root) program ;
  List.iter (analyze_declaration Env.root) program ;
  Output.raise_errors () ;
  Output.report_warnings json ;
  program

and hoist_declaration env = function
  | DComment _ ->
      ()
  | DValueBinding (span, {id; _}) ->
      Env.add_value env id span
  | DTypeDefinition (span, {id; _}) ->
      Env.add_type env id span
  | DADTDefinition (span, {id; variants; _}) ->
      Env.add_type env id span ;
      List.iter
        (fun ({span; id; _} : variant) -> Env.add_variant env id span)
        variants
  | DFunctionDefinition _ ->
      failwith "Function definitions should be desugared before analysis."

and analyze_declaration env = function
  | DComment _ ->
      ()
  | DValueBinding (_, {signature; body; _}) ->
      Option.iter (analyze_typing env) signature ;
      analyze_expression env body
  | DTypeDefinition (_, {id; body}) ->
      analyze_typing ~parent_id:id env body
  | DADTDefinition (span, {id; polymorphics; variants}) ->
      let adt_env = Env.scope env in
      List.iter (fun p -> Env.add_type adt_env p span) polymorphics ;
      List.iter
        (fun ({typing; _} : variant) ->
          Option.iter (analyze_typing ~in_adt:true ~parent_id:id adt_env) typing )
        variants
  | DFunctionDefinition _ ->
      failwith "Function definitions should be desugared before analysis."

and analyze_typing ?(in_adt = false) ?(parent_id = "") env = function
  | TInt _ | TFloat _ | TBool _ | TString _ | TUnit _ ->
      ()
  | TConstructor (span, {id; typing}) ->
      if
        id = parent_id
        || not (Env.type_exists env id || Env.variant_exists env id)
      then Output.add_error (Output.Undefined {name= id; span}) ;
      Option.iter (analyze_typing ~in_adt ~parent_id env) typing
  | TPolymorphic (span, id) ->
      if in_adt && not (Env.type_exists env id) then
        Output.add_error (Output.Undefined {name= id; span})
  | TTuple (_, typings) ->
      List.iter (analyze_typing ~in_adt ~parent_id env) typings
  | TList (_, typing) ->
      analyze_typing ~in_adt ~parent_id env typing
  | TFunction (_, {l; r}) ->
      analyze_typing ~in_adt ~parent_id env l ;
      analyze_typing ~in_adt ~parent_id env r

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
      let left_env = Env.scope env in
      analyze_pattern left_env l ;
      analyze_pattern env r ;
      Hashtbl.iter (Hashtbl.replace (Env.values env)) (Env.values left_env)

and analyze_parameter env = function
  | ALID (span, id) ->
      Env.add_value env id span
  | ATuple (_, parameters) ->
      List.iter (analyze_parameter env) parameters
