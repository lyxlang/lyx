(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
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
      List.iter (fun y -> add sep ; f y) xs

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
  | "print" ->
      {|List.iter (Printf.printf "%f\n%!")|}
  | _ ->
      "l" ^ string_of_int (String.hash str)

let encode_uid str = "U" ^ string_of_int (String.hash str)

let scoped content = add "(" ; content () ; add ")"

let is_type_decl decl =
  match decl.value with
  | Decl _ | Decls _ | Comment _ ->
      false
  | UnionDecl _ | SynDecl _ ->
      true

let is_entry_point decl =
  match decl.value with
  | Decl {id; params= _; signature= _; body= _} ->
      id.value = Wildcard
  | Decls _ | Comment _ | UnionDecl _ | SynDecl _ ->
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
  List.iter (fun d -> build_decl d ; add_newline ()) ordered ;
  Buffer.contents buffer

and build_decl decl =
  match decl.value with
  | Decl {id; params; signature= _; body} ->
      if id.value = Wildcard then add "let"
      else if !first_bind then (
        add "let rec" ;
        first_bind := false )
      else add "and" ;
      add_space () ;
      build_olid id ;
      add_space () ;
      List.iter (fun p -> build_param p ; add_space ()) params ;
      add "=" ;
      add_space () ;
      build_expr body
  | Decls _ ->
      ()
  | UnionDecl {id; polys; variants} ->
      if !first_type then (
        add "type" ;
        first_type := false )
      else add "and" ;
      add_space () ;
      List.iter
        (fun p ->
          add "'" ;
          add (encode_lid p.value) ;
          add_space () )
        polys ;
      add (encode_lid id.value) ;
      add_space () ;
      add "=" ;
      add_space () ;
      List.iter build_variant variants
  | SynDecl {id; typing} ->
      if !first_type then (
        add "type" ;
        first_type := false )
      else add "and" ;
      add_space () ;
      add (encode_lid id.value) ;
      add_space () ;
      add "=" ;
      add_space () ;
      build_typing typing
  | Comment _ ->
      ()

and build_olid olid =
  match olid.value with Wildcard -> add "_" | L str -> add (encode_lid str)

and build_param param =
  match param.value with
  | PRLID str ->
      add (encode_lid str.value)
  | PRTuple tuple_param ->
      scoped (fun () -> add_list "," build_param tuple_param)

and build_expr expr =
  match expr.value with
  | EParenthesized e ->
      scoped (fun () -> build_expr e)
  | ETyped {body; signature= _} ->
      build_expr body
  | EBoolOp {l; op; r} ->
      build_expr l ;
      add_space () ;
      build_bool_op op ;
      add_space () ;
      build_expr r
  | ECompOp {l; op; r} -> (
    match op.value with
    | OpFeq ->
        scoped (fun () ->
            add "Float.equal" ;
            add_space () ;
            build_expr l ;
            add_space () ;
            build_expr r )
    | OpNFeq ->
        scoped (fun () ->
            add "not (Float.equal" ;
            add_space () ;
            build_expr l ;
            add_space () ;
            build_expr r ;
            add ")" )
    | OpGt | OpGeq | OpLt | OpLeq | OpEq | OpNeq ->
        build_expr l ;
        add_space () ;
        build_comp_op op ;
        add_space () ;
        build_expr r )
  | EPipeOp {l; r} ->
      build_expr l ; add_space () ; add "|>" ; add_space () ; build_expr r
  | EConcatOp {l; r} ->
      build_expr l ;
      add_space () ;
      add "(* :: or ^ *)" ;
      add_space () ;
      build_expr r
  | EAddOp {l; op; r} ->
      build_expr l ;
      add_space () ;
      build_add_op op ;
      add_space () ;
      build_expr r
  | EMulOp {l; op; r} ->
      build_expr l ;
      add_space () ;
      build_mul_op op ;
      add_space () ;
      build_expr r
  | EUnOp {op; body} ->
      build_un_op op ; add_space () ; build_expr body
  | EExpOp {l; r} ->
      scoped (fun () ->
          add "Float.pow" ;
          add_space () ;
          build_expr l ;
          add_space () ;
          build_expr r )
  | EBitOp {l; op; r} ->
      scoped (fun () ->
          build_bit_op op ;
          add_space () ;
          build_expr l ;
          add_space () ;
          build_expr r )
  | EApp {fn; arg} ->
      build_expr fn ; add_space () ; build_expr arg
  | ELambda {params; body} ->
      add "fun" ;
      add_space () ;
      List.iter (fun p -> build_param p ; add_space ()) params ;
      add_space () ;
      add "->" ;
      add_space () ;
      build_expr body
  | EMatch {ref; cases} ->
      add "match" ;
      add_space () ;
      build_expr ref ;
      add_space () ;
      add "with" ;
      List.iter build_case cases
  | ELets {binds; body} ->
      List.iter (fun b -> build_bind b) binds ;
      build_expr body
  | ELet _ ->
      ()
  | EIf {predicate; truthy; falsy} ->
      add "if" ;
      add_space () ;
      build_expr predicate ;
      add_space () ;
      add "then" ;
      add_space () ;
      build_expr truthy ;
      add_space () ;
      add "else" ;
      add_space () ;
      build_expr falsy
  | EUID str ->
      add (encode_uid str.value)
  | ELID str ->
      add (encode_lid str.value)
  | ETuple exprs ->
      scoped (fun () -> add_list "," build_expr exprs)
  | EList exprs ->
      add "[" ;
      add_list ";" build_expr exprs ;
      add "]"
  | EUnit ->
      add "()"
  | EBool b ->
      add (string_of_bool b.value)
  | EString s ->
      add "\"" ;
      add (String.escaped s.value) ;
      add "\""
  | EFloat f | EInt f ->
      add (string_of_float (float_of_string f.value))

and build_bool_op op =
  match op.value with OpBoolAnd -> add "&&" | OpBoolOr -> add "||"

and build_comp_op op =
  match op.value with
  | OpGt ->
      add ">"
  | OpGeq ->
      add ">="
  | OpLt ->
      add "<"
  | OpLeq ->
      add "<="
  | OpEq ->
      add "="
  | OpFeq | OpNFeq ->
      ()
  | OpNeq ->
      add "<>"

and build_add_op op =
  match op.value with OpAdd -> add "+." | OpSub -> add "-."

and build_mul_op op =
  match op.value with
  | OpMul ->
      add "*."
  | OpDiv ->
      add "/."
  | OpMod ->
      add "mod"

and build_un_op op =
  match op.value with
  | UnPlus ->
      add "+."
  | UnNeg ->
      add "-."
  | UnBoolNot ->
      add "not"

and build_bit_op op =
  match op.value with
  | OpBitLShift ->
      add
        "(fun l r -> int_of_float l |> Int.shift_left (int_of_float r) |> \
         float_of_int)"
  | OpBitRShift ->
      add
        "(fun l r -> int_of_float l |> Int.shift_right_logical (int_of_float \
         r) |> float_of_int)"
  | OpBitAnd ->
      add
        "(fun l r -> int_of_float l |> Int.logand (int_of_float r) |> \
         float_of_int)"
  | OpBitOr ->
      add
        "(fun l r -> int_of_float l |> Int.logor (int_of_float r) |> \
         float_of_int)"
  | OpBitXor ->
      add
        "(fun l r -> int_of_float l |> Int.logxor (int_of_float r) |> \
         float_of_int)"

and build_bind bind =
  add "let" ;
  add_space () ;
  add (encode_lid bind.value.id.value) ;
  add_space () ;
  List.iter (fun p -> build_param p ; add_space ()) bind.value.params ;
  add_space () ;
  add "=" ;
  add_space () ;
  build_expr bind.value.body ;
  add_space () ;
  add "in" ;
  add_space ()

and build_case case =
  match case.value with
  | Case {pat; body} ->
      add_space () ;
      add "|" ;
      add_space () ;
      build_pat pat ;
      add_space () ;
      add "->" ;
      add_space () ;
      build_expr body
  | CaseGuard {pat; guard; body} ->
      add_space () ;
      add "|" ;
      add_space () ;
      build_pat pat ;
      add_space () ;
      add "when" ;
      add_space () ;
      build_expr guard ;
      add_space () ;
      add "->" ;
      add_space () ;
      build_expr body

and build_pat pat =
  match pat.value with
  | PInt str | PFloat str ->
      add (string_of_float (float_of_string str.value))
  | PString s ->
      add "\"" ;
      add (String.escaped s.value) ;
      add "\""
  | PBool b ->
      add (string_of_bool b.value)
  | POLID str | PTail str ->
      build_olid str
  | PConstructor {id; params} ->
      add (encode_uid id.value) ;
      add_space () ;
      List.iter (fun p -> build_pat p ; add_space ()) params
  | PList list_pat ->
      add "[" ;
      add_list ";" build_pat list_pat.value ;
      add "]"
  | PListSpd list_spd_pat ->
      add_list "::" build_pat list_spd_pat.value
  | PTuple tuple_pat ->
      scoped (fun () -> add_list "," build_pat tuple_pat.value)
  | POr {l; r} ->
      build_pat l ; add_space () ; add "|" ; add_space () ; build_pat r
  | PParenthesized p ->
      build_pat p

and build_variant variant =
  add "|" ;
  add_space () ;
  add (encode_uid variant.value.id.value) ;
  if variant.value.typings <> [] then (
    add_space () ;
    add "of" ;
    add_space () ;
    build_variant_typings variant.value.typings ) ;
  add_space ()

and build_variant_typings typings =
  match typings with
  | [t] ->
      build_typing t
  | _ ->
      add_list "*" build_typing typings

and build_typing t =
  match t.value with
  | TInt ->
      add "float"
  | TFloat ->
      add "float"
  | TString ->
      add "string"
  | TBool ->
      add "bool"
  | TUnit ->
      add "unit"
  | TList typing ->
      build_typing typing ; add_space () ; add "list"
  | TTuple ts ->
      add_list "*" build_typing ts
  | TFunc {l; r} ->
      build_typing l ; add_space () ; add "->" ; add_space () ; build_typing r
  | TPoly p ->
      add "'" ;
      add (encode_lid p.value)
  | TConstructor {id; typings} ->
      add (encode_uid id.value) ;
      if typings <> [] then (
        add_space () ;
        add "of" ;
        add_space () ;
        build_variant_typings typings )
  | TTyping t' ->
      scoped (fun () -> build_typing t')
