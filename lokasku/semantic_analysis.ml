(*
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

type arity = int [@@deriving show {with_path= false}]

type info = span * arity

type name = string [@@deriving show {with_path= false}]

type map = (name, info) Hashtbl.t

type scope = Scope of map * scope | Root

type error =
  | AlreadyDefined of {prev: span; new': name; newest_span: span}
  | Undefined of {name: name; span: span}
  | ReservedName of {name: name; span: span}
  | NotACallee of {name: name; span: span}
  | ArityMismatch of {name: name; expected: arity; span: span}
[@@deriving show {with_path= false}]

type warning = Unused of {name: name; span: span}
[@@deriving show {with_path= false}]

type analysis_output =
  { mutable errors: error list
  ; mutable warnings: (name, warning) Hashtbl.t
  ; mutable types: map
  ; mutable variants: map }

let output =
  { errors= []
  ; warnings= Hashtbl.create 100
  ; types= Hashtbl.create 100
  ; variants= Hashtbl.create 500 }

let builtins = ["Int"; "Float"; "String"; "Bool"; "Unit"]

let stdlib = Hashtbl.create 20

let _ =
  List.iter
    (fun (name, arity) -> Hashtbl.add stdlib name ({start= 0; fin= 0}, arity))
    [ ("sqrt", 1)
    ; ("\u{03C0}", 0)
    ; ("map", 2)
    ; ("filter", 2)
    ; ("foldLeft", 3)
    ; ("foldRight", 3)
    ; ("printString", 1)
    ; ("printNumber", 1) ]

let new_map () = Hashtbl.create 100

let rec find_identifier scope name =
  match scope with
  | Root ->
      Hashtbl.find_opt stdlib name
  | Scope (map, parent) -> (
    match Hashtbl.find_opt map name with
    | Some info ->
        Some info
    | None ->
        find_identifier parent name )

let add_identifier scope name info =
  match scope with
  | Scope (map, _) ->
      Hashtbl.replace map name info
  | Root ->
      assert false

let rec analyze_parameter scope param =
  match param with
  | ALID (span, lid) ->
      if not (String.starts_with lid ~prefix:"_") then (
        add_identifier scope lid (span, 0) ;
        Hashtbl.add output.warnings lid (Unused {name= lid; span}) )
  | ATuple (_, lst) ->
      List.iter (fun param -> analyze_parameter scope param) lst

let rec analyze_pattern scope pattern =
  match pattern with
  | PInt _ | PFloat _ | PBool _ | PString _ ->
      ()
  | PLID (span, lid) ->
      if not (String.starts_with lid ~prefix:"_") then (
        add_identifier scope lid (span, 0) ;
        Hashtbl.add output.warnings lid (Unused {name= lid; span}) )
  | PConstructor (span, {id; pattern}) -> (
      ( match Hashtbl.find_opt output.variants id with
      | Some _ ->
          ()
      | None ->
          output.errors <- Undefined {name= id; span} :: output.errors ) ;
      match pattern with Some p -> analyze_pattern scope p | None -> () )
  | PTuple (_, lst) | PList (_, lst) | PListSpread (_, lst) ->
      List.iter (fun pattern -> analyze_pattern scope pattern) lst
  | POr (_, {l; r}) ->
      analyze_pattern scope l ;
      analyze_pattern scope r ;
      let rec vars_collected pattern =
        match pattern with
        | PInt _ | PFloat _ | PString _ | PBool _ ->
            []
        | PLID (_, lid) ->
            [lid]
        | PTuple (_, lst) | PList (_, lst) | PListSpread (_, lst) ->
            List.concat_map vars_collected lst
        | PConstructor (_, {pattern= Some p; _}) ->
            vars_collected p
        | PConstructor (_, {pattern= None; _}) ->
            []
        | POr (_, {l; r}) ->
            vars_collected l @ vars_collected r
      in
      List.iter
        (fun var -> Hashtbl.remove output.warnings var)
        (vars_collected l @ vars_collected r)

let rec analyze_type typing =
  match typing with
  | TList (_, t) ->
      analyze_type t
  | TTuple (_, lst) ->
      List.iter (fun typing -> analyze_type typing) lst
  | TFunction (_, {l; r}) ->
      analyze_type l ; analyze_type r
  | TConstructor (span, {id; typing}) -> (
    match Hashtbl.find_opt output.types id with
    | Some (span, arity) ->
        let expected_arity = if Option.is_some typing then 1 else 0 in
        if expected_arity <> arity then
          output.errors <-
            ArityMismatch {name= id; expected= arity; span} :: output.errors ;
        Hashtbl.remove output.warnings id
    | None ->
        output.errors <- Undefined {name= id; span} :: output.errors )
  | TPolymorphic _ | TInt _ | TFloat _ | TString _ | TBool _ | TUnit _ ->
      ()

let rec check_poly_pattern pattern =
  match pattern with
  | PInt _ | PFloat _ | PBool _ | PString _ | PLID _ ->
      None
  | PConstructor (_, {id; pattern= p}) -> (
    match Hashtbl.find_opt output.types id with
    | Some (span, arity) when arity > 0 ->
        Some (id, span)
    | _ ->
        Option.bind p check_poly_pattern )
  | PTuple (_, lst) | PList (_, lst) | PListSpread (_, lst) ->
      List.find_map check_poly_pattern lst
  | POr (_, {l; r}) -> (
    match check_poly_pattern l with
    | Some res ->
        Some res
    | None ->
        check_poly_pattern r )

(* Check if the pattern has a guard to avoid undertermined behavior in the type
   checking *)
let rec analyze_case scope (case : case) =
  let scope' = Scope (new_map (), scope) in
  analyze_pattern scope' case.pattern ;
  (* si polymorphique ... *)
  ( match check_poly_pattern case.pattern with
  | Some (name, loc) when Option.is_none case.guard ->
      (* si aucune guard, erreur car ambiguitée au typechecking *)
      output.errors <-
        ArityMismatch {name; expected= 1; span= loc} :: output.errors
  | _ ->
      () ) ;
  Option.iter (analyze_expression scope') case.guard ;
  analyze_expression scope' case.body

and analyze_binding scope ({span; id; signature; body} : binding) =
  (match signature with Some sign -> analyze_type sign | None -> ()) ;
  if not (String.starts_with id ~prefix:"_") then
    Hashtbl.add output.warnings id (Unused {name= id; span}) ;
  analyze_expression scope body

and analyze_expression scope expr =
  match expr with
  | EInt _ | EFloat _ | EBool _ | EString _ | EUnit _ ->
      ()
  | EConstructor (span, {id; body}) -> (
      ( match Hashtbl.find_opt output.variants id with
      | Some _ ->
          Hashtbl.remove output.warnings id
      | None ->
          output.errors <- Undefined {name= id; span} :: output.errors ) ;
      match body with None -> () | Some b -> analyze_expression scope b )
  | ELID (span, lid) -> (
    match find_identifier scope lid with
    | Some _ ->
        Hashtbl.remove output.warnings lid
    | None ->
        output.errors <- Undefined {name= lid; span} :: output.errors )
  | ETuple (_, lst) | EList (_, lst) ->
      List.iter (fun expr -> analyze_expression scope expr) lst
  | EBinaryOperation (_, {l; r; _}) ->
      analyze_expression scope l ; analyze_expression scope r
  | EUnaryOperation (_, {body; _}) ->
      analyze_expression scope body
  | EApplication (_, {body; argument}) ->
      analyze_expression scope body ;
      analyze_expression scope argument
  | ELambda (_, {parameters; body}) ->
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param) parameters ;
      analyze_expression scope' body
  | EMatch (_, {body; cases}) ->
      analyze_expression scope body ;
      List.iter (fun case -> analyze_case scope case) cases
  | ELet (_, {bindings; body}) ->
      let scope' = Scope (new_map (), scope) in
      List.iter
        (fun ({span; id; _} : binding) ->
          match find_identifier scope id with
          | Some (span', _) ->
              output.errors <-
                AlreadyDefined {prev= span'; new'= id; newest_span= span}
                :: output.errors
          | None ->
              add_identifier scope id (span, 0) )
        bindings ;
      List.iter
        (fun binding ->
          let scope'' = Scope (new_map (), scope') in
          analyze_binding scope'' binding )
        bindings ;
      analyze_expression scope' body
  | EIf (_, {predicate; truthy; falsy}) ->
      analyze_expression scope predicate ;
      analyze_expression scope truthy ;
      analyze_expression scope falsy
  | EExpression (_, {body; signature}) -> (
      analyze_expression scope body ;
      match signature with Some typ -> analyze_type typ | None -> () )

let rec analyze_variant_type scope typing =
  match typing with
  | TPolymorphic (span, lid) -> (
    match find_identifier scope lid with
    | Some _ ->
        Hashtbl.remove output.warnings lid
    | None ->
        output.errors <- Undefined {name= lid; span} :: output.errors )
  | TList (_, t) ->
      analyze_variant_type scope t
  | TTuple (_, lst) ->
      List.iter (fun typing -> analyze_variant_type scope typing) lst
  | TFunction (_, {l; r}) ->
      analyze_variant_type scope l ;
      analyze_variant_type scope r
  | TConstructor (span, {id; typing}) -> (
    match Hashtbl.find_opt output.types id with
    | Some (span, arity) ->
        let expected_arity = if Option.is_some typing then 1 else 0 in
        if expected_arity <> arity then
          output.errors <-
            ArityMismatch {name= id; expected= arity; span} :: output.errors ;
        Hashtbl.remove output.warnings id
    | None ->
        output.errors <- Undefined {name= id; span} :: output.errors )
  | TInt _ | TFloat _ | TString _ | TBool _ | TUnit _ ->
      ()

and analyze_variant scope (variant : variant) =
  match Hashtbl.find_opt output.variants variant.id with
  | Some (variant_span, _) ->
      output.errors <-
        AlreadyDefined
          {prev= variant_span; new'= variant.id; newest_span= variant.span}
        :: output.errors
  | None ->
      ( match variant.typing with
      | Some typing ->
          analyze_variant_type scope typing
      | None ->
          () ) ;
      Hashtbl.add output.variants variant.id
        (variant.span, if Option.is_some variant.typing then 1 else 0) ;
      Hashtbl.add output.warnings variant.id
        (Unused {name= variant.id; span= variant.span})

let analyze_declaration scope decl =
  match decl with
  | DFunctionDefinition (span, {id; parameters; signature; body}) ->
      (match signature with Some sign -> analyze_type sign | None -> ()) ;
      ( match find_identifier scope id with
      | Some (span', _) ->
          output.errors <-
            AlreadyDefined {prev= span'; new'= id; newest_span= span}
            :: output.errors
      | None ->
          if not (String.starts_with id ~prefix:"_") then (
            add_identifier scope id (span, List.length parameters) ;
            Hashtbl.add output.warnings id (Unused {name= id; span}) ) ) ;
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param) parameters ;
      analyze_expression scope' body
  | DValueBinding (span, binding) ->
      let id = binding.id in
      ( match find_identifier scope id with
      | Some (span', _) ->
          output.errors <-
            AlreadyDefined {prev= span'; new'= id; newest_span= binding.span}
            :: output.errors
      | None ->
          if not (String.starts_with id ~prefix:"_") then (
            add_identifier scope id (span, 0) ;
            Hashtbl.add output.warnings id (Unused {name= id; span}) ) ) ;
      (match binding.signature with Some typ -> analyze_type typ | None -> ()) ;
      analyze_expression scope binding.body
  | DADTDefinition (span, {id; polymorphics; variants}) ->
      (* Name *)
      ( match Hashtbl.find_opt output.types id with
      (* Was not already defined *)
      | Some (span', _) ->
          output.errors <-
            AlreadyDefined {prev= span'; new'= id; newest_span= span}
            :: output.errors
      | None -> (
        match List.exists (fun builtin -> builtin = id) builtins with
        (* Is not a reserved name *)
        | true ->
            output.errors <- ReservedName {name= id; span} :: output.errors
        | false ->
            Hashtbl.add output.types id (span, List.length polymorphics) ) ) ;
      (* Polymorphic variables *)
      let scope' = Scope (new_map (), Root) in
      List.iter
        (fun poly ->
          match find_identifier scope' poly with
          | Some (span', _) ->
              output.errors <-
                AlreadyDefined {prev= span'; new'= poly; newest_span= span}
                :: output.errors
          | None ->
              add_identifier scope' poly (span, List.length polymorphics) )
        polymorphics ;
      (* Variants *)
      List.iter (fun variant -> analyze_variant scope' variant) variants
  | DTypeDefinition (span, {id; body}) -> (
      analyze_type body ;
      match Hashtbl.find_opt output.types id with
      | Some (span', _) ->
          output.errors <-
            AlreadyDefined {prev= span'; new'= id; newest_span= span}
            :: output.errors
      | None ->
          Hashtbl.add output.types id (span, 0) )
  | DComment _ ->
      ()

let analyze_program (program : Ast.program) =
  let root_scope = Scope (new_map (), Root) in
  List.iter (fun decl -> analyze_declaration root_scope decl) program ;
  output

let debug_output output =
  List.iter
    (fun error -> Printf.printf "\n\027[31m%s\027[0m" (show_error error))
    output.errors ;
  Hashtbl.iter
    (fun _ warning -> Printf.printf "\n\027[33m%s\027[0m" (show_warning warning))
    output.warnings ;
  Printf.printf "\n%!"

let get_errors output = output.errors
