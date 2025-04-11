(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

type arity = int

type info = span * arity

type name = string

type map = (name, info) Hashtbl.t

type scope = Scope of map * scope | Root

type error =
  | AlreadyDefined of {prev: span; new': string; newest_span: span}
  | Undefined of {name: string; span: span}
  | ReservedName of {name: string; span: span}
  | NotACallee of {name: string; span: span}
  | ArityMismatch of {name: string; expected: int; span: span}
[@@deriving show]

type warning = Unused of {name: string; span: span} [@@deriving show]

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
  | PRLID lid ->
      add_identifier scope lid.value (lid.loc, 0) ;
      Hashtbl.add output.warnings lid.value
        (Unused {name= lid.value; span= lid.loc})
  | PRTuple lst ->
      List.iter (fun param -> analyze_parameter scope param.value) lst

let rec analyze_pattern scope pattern =
  match pattern with
  | PInt _ | PFloat _ | PString _ | PBool _ ->
      ()
  | POLID olid | PTail olid -> (
    match olid.value with
    | Wildcard ->
        ()
    | L lid ->
        add_identifier scope lid (olid.loc, 0) ;
        Hashtbl.add output.warnings lid (Unused {name= lid; span= olid.loc}) )
  | PConstructor {id; params} ->
      ( match Hashtbl.find_opt output.variants id.value with
      | Some _ ->
          ()
      | None ->
          output.errors <-
            Undefined {name= id.value; span= id.loc} :: output.errors ) ;
      List.iter (fun pattern -> analyze_pattern scope pattern.value) params
  | PTuple lst | PList lst | PListSpd lst ->
      List.iter (fun pattern -> analyze_pattern scope pattern.value) lst.value
  | POr {l; r} ->
      analyze_pattern scope l.value ;
      analyze_pattern scope r.value
  | PParenthesized p ->
      analyze_pattern scope p.value

let rec analyze_type typing =
  match typing with
  | TList t ->
      analyze_type t.value
  | TTuple lst ->
      List.iter (fun typing -> analyze_type typing.value) lst
  | TFunc {l; r} ->
      analyze_type l.value ; analyze_type r.value
  | TConstructor variant -> (
    match Hashtbl.find_opt output.types variant.id.value with
    | Some (span, arity) ->
        let expected_arity = if Option.is_some variant.typing then 1 else 0 in
        if expected_arity <> arity then
          output.errors <-
            ArityMismatch {name= variant.id.value; expected= arity; span}
            :: output.errors ;
        Hashtbl.remove output.warnings variant.id.value
    | None ->
        output.errors <-
          Undefined {name= variant.id.value; span= variant.id.loc}
          :: output.errors )
  | TTyping t ->
      analyze_type t.value
  | TInt | TFloat | TString | TBool | TUnit | TPoly _ ->
      ()

let rec analyze_case scope case =
  match case with
  | Case {pat; body} ->
      (* TODO: Check that polymorphic types appear in every predicate *)
      (*
      let scope' =
        List.fold_left
          (fun acc predicate ->
            let scope' = Scope (new_map (), acc) in
            analyze_pattern scope' predicate.value ;
            scope' )
          (Scope (new_map (), Root))
          predicates
      in
      *)
      let scope' = Scope (new_map (), scope) in
      analyze_pattern scope' pat.value ;
      analyze_expression scope' body.value
  | CaseGuard {pat; guard; body} ->
      let scope' = Scope (new_map (), scope) in
      analyze_pattern scope' pat.value ;
      analyze_expression scope' guard.value ;
      analyze_expression scope' body.value

and analyze_bind scope ({id; params; signature; body} : bind) =
  List.iter (fun param -> analyze_parameter scope param.value) params ;
  Hashtbl.add output.warnings id.value (Unused {name= id.value; span= id.loc}) ;
  if Option.is_some signature then analyze_type (Option.get signature).value ;
  analyze_expression scope body.value

and analyze_expression scope expr =
  match expr with
  | EParenthesized e ->
      analyze_expression scope e.value
  | ETyped {body; signature} ->
      analyze_expression scope body.value ;
      if Option.is_some signature then analyze_type (Option.get signature).value
  | EBoolOp {l; r; _}
  | ECompOp {l; r; _}
  | EAddOp {l; r; _}
  | EMulOp {l; r; _}
  | EBitOp {l; r; _}
  | EPipeOp {l; r}
  | EConcatOp {l; r}
  | EExpOp {l; r} ->
      analyze_expression scope l.value ;
      analyze_expression scope r.value
  | EUnOp {body; _} ->
      analyze_expression scope body.value
  | EApp {fn; arg} ->
      analyze_expression scope fn.value ;
      analyze_expression scope arg.value
  | ELambda {params; body} ->
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param.value) params ;
      analyze_expression scope' body.value
  | EMatch {ref; cases} ->
      analyze_expression scope ref.value ;
      List.iter (fun case -> analyze_case scope case.value) cases
  | ELets {binds; body} ->
      let scope' = Scope (new_map (), scope) in
      List.iter
        (fun ({value= {id; params; _}; _} : bind located) ->
          match find_identifier scope id.value with
          | Some (span, _) ->
              output.errors <-
                AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
                :: output.errors
          | None ->
              add_identifier scope id.value (id.loc, List.length params) )
        binds ;
      List.iter
        (fun bind ->
          let scope'' = Scope (new_map (), scope') in
          analyze_bind scope'' bind.value )
        binds ;
      analyze_expression scope' body.value
  | ELet _ ->
      assert false
  | EIf {predicate; truthy; falsy} ->
      analyze_expression scope predicate.value ;
      analyze_expression scope truthy.value ;
      analyze_expression scope falsy.value
  | EUID uid -> (
    match Hashtbl.find_opt output.variants uid.value with
    | Some _ ->
        Hashtbl.remove output.warnings uid.value
    | None ->
        output.errors <-
          Undefined {name= uid.value; span= uid.loc} :: output.errors )
  | ELID lid -> (
    match find_identifier scope lid.value with
    | Some _ ->
        Hashtbl.remove output.warnings lid.value
    | None ->
        output.errors <-
          Undefined {name= lid.value; span= lid.loc} :: output.errors )
  | ETuple lst | EList lst ->
      List.iter (fun expr -> analyze_expression scope expr.value) lst
  | EInt _ | EFloat _ | EString _ | EBool _ | EUnit ->
      ()

let rec analyze_variant_type scope typing =
  match typing with
  | TPoly lid -> (
    match find_identifier scope lid.value with
    | Some _ ->
        Hashtbl.remove output.warnings lid.value
    | None ->
        output.errors <-
          Undefined {name= lid.value; span= lid.loc} :: output.errors )
  | TList t ->
      analyze_variant_type scope t.value
  | TTuple lst ->
      List.iter (fun typing -> analyze_variant_type scope typing.value) lst
  | TFunc {l; r} ->
      analyze_variant_type scope l.value ;
      analyze_variant_type scope r.value
  | TConstructor {id; typing} -> (
    match Hashtbl.find_opt output.types id.value with
    | Some (_, arity) ->
        let expected_arity = if Option.is_some typing then 1 else 0 in
        if expected_arity <> arity then
          output.errors <-
            ArityMismatch {name= id.value; expected= arity; span= id.loc}
            :: output.errors ;
        Hashtbl.remove output.warnings id.value
    | None ->
        output.errors <-
          Undefined {name= id.value; span= id.loc} :: output.errors )
  | TTyping t ->
      analyze_variant_type scope t.value
  | TInt | TFloat | TString | TBool | TUnit ->
      ()

and analyze_variant scope (variant : variant) =
  match Hashtbl.find_opt output.variants variant.id.value with
  | Some (variant_span, _) ->
      output.errors <-
        AlreadyDefined
          { prev= variant_span
          ; new'= variant.id.value
          ; newest_span= variant.id.loc }
        :: output.errors
  | None ->
      ( match variant.typing with
      | Some typing ->
          analyze_variant_type scope typing.value
      | None ->
          () ) ;
      Hashtbl.add output.variants variant.id.value
        (variant.id.loc, if Option.is_some variant.typing then 1 else 0) ;
      Hashtbl.add output.warnings variant.id.value
        (Unused {name= variant.id.value; span= variant.id.loc})

let analyze_declaration scope decl =
  match decl with
  | Decl {id; params; signature; body} ->
      (match signature with Some sign -> analyze_type sign.value | None -> ()) ;
      ( match id.value with
      | Wildcard ->
          ()
      | L lid -> (
        match find_identifier scope lid with
        | Some (span, _) ->
            output.errors <-
              AlreadyDefined {prev= span; new'= lid; newest_span= id.loc}
              :: output.errors
        | None ->
            add_identifier scope lid (id.loc, List.length params) ;
            Hashtbl.add output.warnings lid (Unused {name= lid; span= id.loc}) )
      ) ;
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param.value) params ;
      analyze_expression scope' body.value
  | Decls _ ->
      assert false
  | UnionDecl {id; polys; variants} ->
      (* Name *)
      ( match Hashtbl.find_opt output.types id.value with
      (* Was not already defined *)
      | Some (span, _) ->
          output.errors <-
            AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
            :: output.errors
      | None -> (
        match List.exists (fun builtin -> builtin = id.value) builtins with
        (* Is not a reserved name *)
        | true ->
            output.errors <-
              ReservedName {name= id.value; span= id.loc} :: output.errors
        | false ->
            Hashtbl.add output.types id.value (id.loc, List.length polys) ) ) ;
      (* Polymorphic variables *)
      let scope' = Scope (new_map (), Root) in
      List.iter
        (fun poly ->
          match find_identifier scope' poly.value with
          | Some (span, _) ->
              output.errors <-
                AlreadyDefined
                  {prev= span; new'= poly.value; newest_span= poly.loc}
                :: output.errors
          | None ->
              add_identifier scope' poly.value (poly.loc, List.length polys) ;
              Hashtbl.add output.warnings poly.value
                (Unused {name= poly.value; span= poly.loc}) )
        polys ;
      (* Variants *)
      List.iter (fun variant -> analyze_variant scope' variant.value) variants
  | SynDecl {id; typing} -> (
      analyze_type typing.value ;
      match Hashtbl.find_opt output.types id.value with
      | Some (span, _) ->
          output.errors <-
            AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
            :: output.errors
      | None ->
          Hashtbl.add output.types id.value (id.loc, 0) )
  | Comment _ ->
      ()

let analyze_program (program : Ast.program) =
  let root_scope = Scope (new_map (), Root) in
  List.iter (fun decl -> analyze_declaration root_scope decl.value) program ;
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
