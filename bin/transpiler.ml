(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lys.Ast

let first_type = ref true

let first_bind = ref true

let buffer = Buffer.create 1024

let add s = Buffer.add_string buffer s

let add_space () = Buffer.add_char buffer ' '

let add_newline () = Buffer.add_char buffer '\n'

let add_list sep f lst =
  match lst with
  | [] ->
      ()
  | [x] ->
      f x
  | x :: xs ->
      f x ;
      List.iter (add sep ; f) xs

let encode_lid str =
  match str with
  | "sqrt" ->
      "Float.sqrt"
  | "\u{03C0}" ->
      "Float.pi"
  | "map" ->
      "List.map"
  | "filter" ->
      "List.filter"
  | "foldLeft" ->
      "List.fold_left"
  | "foldRight" ->
      "List.fold_right"
  | "printString" ->
      "print_endline"
  | "printNumber" ->
      {|(Format.printf "%f@.")|}
  | _ ->
      "l" ^ string_of_int (String.hash str)

let encode_uid str = "U" ^ string_of_int (String.hash str)

let scoped content = add "(" ; content () ; add ")"

let is_type_decl decl =
  match decl with
  | DComment _ | DValueBinding _ | DFunctionDefinition _ ->
      false
  | DTypeDefinition _ | DADTDefinition _ ->
      true

let is_entry_point decl =
  match decl with
  | DValueBinding (_, {id; _}) ->
      id = "_"
  | DComment _ | DFunctionDefinition _ | DTypeDefinition _ | DADTDefinition _ ->
      false

let rec build_program decls =
  Buffer.clear buffer ;
  first_type := true ;
  first_bind := true ;
  let type_decls, non_type_decls = List.partition is_type_decl decls in
  let entry_point_decls, other_decls =
    List.partition is_entry_point non_type_decls
  in
  let ordered = type_decls @ other_decls @ entry_point_decls in
  List.iter (fun d -> build_declaration d ; add_newline ()) ordered ;
  Buffer.contents buffer

and build_declaration declaration =
  match declaration with
  | DComment _ ->
      ()
  | DValueBinding (_, {id; body; _}) ->
      if id = "_" then add "let"
      else if !first_bind then (
        add "let rec" ;
        first_bind := false )
      else add "and" ;
      add_space () ;
      add @@ encode_lid id ;
      add_space () ;
      add "=" ;
      add_space () ;
      build_expression body
  | DTypeDefinition (_, {id; body}) ->
      if !first_type then (
        add "type" ;
        first_type := false )
      else add "and" ;
      add_space () ;
      add @@ encode_lid id ;
      add_space () ;
      add "=" ;
      add_space () ;
      build_typing body
  | DFunctionDefinition _ ->
      failwith "Function definitions should be desugared before transpiling."
  | DADTDefinition (_, {id; polymorphics; variants}) ->
      if !first_type then (
        add "type" ;
        first_type := false )
      else add "and" ;
      add_space () ;
      List.iter
        (fun p ->
          add "'" ;
          add @@ encode_lid p ;
          add_space () )
        polymorphics ;
      add @@ encode_lid id ;
      add_space () ;
      add "=" ;
      add_space () ;
      add_list " " build_variant variants

and build_parameter parameter =
  match parameter with
  | ALID (_, str) ->
      add @@ encode_lid str
  | ATuple (_, tuple_param) ->
      scoped (fun () -> add_list "," build_parameter tuple_param)

and build_typing typing =
  match typing with
  | TInt _ ->
      add "float"
  | TFloat _ ->
      add "float"
  | TString _ ->
      add "string"
  | TBool _ ->
      add "bool"
  | TUnit _ ->
      add "unit"
  | TList (_, t) ->
      build_typing t ; add_space () ; add "list"
  | TTuple (_, ts) ->
      add_list "*" build_typing ts
  | TFunction (_, {l; r}) ->
      build_typing l ; add_space () ; add "->" ; add_space () ; build_typing r
  | TPolymorphic (_, p) ->
      add "'" ;
      add @@ encode_lid p
  | TConstructor (_, {id; typing}) -> (
      add @@ encode_lid id ;
      match typing with
      | Some t ->
          add_space () ; add "of" ; add_space () ; build_typing t
      | None ->
          () )

and build_expression expr =
  match expr with
  | EExpression (_, {body; signature= _}) ->
      scoped (fun () -> build_expression body)
  | EInt (_, str) ->
      float_of_string str |> string_of_float |> add
  | EFloat (_, str) ->
      add str
  | EBool (_, b) ->
      add @@ string_of_bool b
  | EString (_, s) ->
      add "\"" ;
      add @@ String.escaped s ;
      add "\""
  | EUnit _ ->
      add "()"
  | EConstructor (_, {id; body}) -> (
      add @@ encode_uid id ;
      match body with None -> () | Some b -> add_space () ; build_expression b )
  | ELID (_, id) ->
      add @@ encode_lid id
  | ETuple (_, exprs) ->
      scoped (fun () -> add_list "," build_expression exprs)
  | EList (_, exprs) ->
      add "[" ;
      add_list ";" build_expression exprs ;
      add "]"
  | EBinaryOperation (_, {l; operator; r}) ->
      build_expression l ;
      add_space () ;
      build_binary_operator operator ;
      add_space () ;
      build_expression r
  | EUnaryOperation (_, {operator; body}) ->
      build_unary_operator operator ;
      add_space () ;
      build_expression body
  | ELet _ ->
      failwith "Let expressions should be desugared before transpiling."
  | EDesugaredLet (_, {binding; body}) ->
      build_binding binding ; build_expression body
  | EIf (_, {predicate; truthy; falsy}) ->
      add "if" ;
      add_space () ;
      build_expression predicate ;
      add_space () ;
      add "then" ;
      add_space () ;
      build_expression truthy ;
      add_space () ;
      add "else" ;
      add_space () ;
      build_expression falsy
  | EMatch (_, {body; cases}) ->
      add "match" ;
      add_space () ;
      build_expression body ;
      add_space () ;
      add "with" ;
      add_space () ;
      add_list " " build_case cases
  | ELambda _ ->
      failwith "Lambdas should be desugared before transpiling."
  | EDesugaredLambda (_, {parameter; body}) ->
      add "fun" ;
      add_space () ;
      build_parameter parameter ;
      add_space () ;
      add "->" ;
      add_space () ;
      build_expression body
  | EApplication (_, {body; argument}) ->
      build_expression body ; add_space () ; build_expression argument

and build_binary_operator op =
  match op with
  | BPipe ->
      add "|>"
  | BOr ->
      add "||"
  | BAnd ->
      add "&&"
  | BEqual ->
      add "="
  | BNotEqual ->
      add "<>"
  | BGreaterThan ->
      add ">"
  | BGreaterOrEqual ->
      add ">="
  | BLessThan ->
      add "<"
  | BLessOrEqual ->
      add "<="
  | BConcatenate ->
      print_endline
        {|\027[31mERROR: The transpiler does not yet support the concatenation operator.\027[0m|} ;
      add "(* TODO: Waiting on type checker. *)"
  | BAdd ->
      add "+."
  | BSubstract ->
      add "-."
  | BMultiply ->
      add "*."
  | BDivide ->
      add "/."
  | BModulo ->
      add "mod"
  | BExponentiate ->
      add "**"

and build_unary_operator op =
  match op with UPlus -> add "+." | UMinus -> add "-." | UNot -> add "not"

and build_binding binding =
  add "let" ;
  add_space () ;
  add @@ encode_lid binding.id ;
  add_space () ;
  add "=" ;
  add_space () ;
  build_expression binding.body ;
  add_space () ;
  add "in" ;
  add_space ()

and build_case case =
  add "|" ;
  add_space () ;
  build_pattern case.pattern ;
  ( match case.guard with
  | Some guard ->
      add_space () ; add "when" ; add_space () ; build_expression guard
  | None ->
      () ) ;
  add_space () ; add "->" ; add_space () ; build_expression case.body

and build_pattern pat =
  match pat with
  | PInt (_, str) ->
      float_of_string str |> string_of_float |> add
  | PFloat (_, str) ->
      add str
  | PBool (_, b) ->
      add @@ string_of_bool b
  | PString (_, s) ->
      add "\"" ;
      add @@ String.escaped s ;
      add "\""
  | PLID (_, lid) ->
      add @@ encode_lid lid
  | PTuple (_, pats) ->
      scoped (fun () -> add_list "," build_pattern pats)
  | PList (_, pats) ->
      add "[" ;
      add_list ";" build_pattern pats ;
      add "]"
  | PListSpread (_, pats) ->
      add_list "::" build_pattern pats
  | PConstructor (_, {id; pattern}) -> (
      add @@ encode_uid id ;
      match pattern with Some p -> add_space () ; build_pattern p | None -> () )
  | POr (_, {l; r}) ->
      build_pattern l ; add_space () ; add "|" ; add_space () ; build_pattern r

and build_variant variant =
  add "|" ;
  add_space () ;
  add @@ encode_uid variant.id ;
  match variant.typing with
  | Some t ->
      add_space () ; add "of" ; add_space () ; build_typing t
  | None ->
      ()
