(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

let limit = 80

let indent = "  "

module IntSet = Set.Make (Int)

module Node = struct
  let unicode_width str =
    String.split_on_char '\n' str
    |> List.hd
    |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n _ -> n + 1) 0

  type t =
    | Nodes of t list
    | Group of int * t list
    | Fill of t list
    | Text of string * int
    | SpaceOrLine
    | Line
    | HardLine
    | EmptyLine
    | Indent of t list
    | IndentNext of t list

  let text str = Text (str, unicode_width str)

  let rec width wrapped = function
    | Nodes nodes
    | Group (_, nodes)
    | Fill nodes
    | Indent nodes
    | IndentNext nodes ->
        List.fold_left (fun acc node -> acc + width wrapped node) 0 nodes
    | Text (_, width) ->
        width
    | HardLine | EmptyLine ->
        limit
    | SpaceOrLine ->
        1
    | Line ->
        0
end

module Wrap = struct
  type t = Detect | Enable | Disable | Force
end

module Generator = struct
  open Node

  type t =
    { buffer: Buffer.t
    ; mutable depth: int
    ; mutable size: int
    ; mutable wrapped: IntSet.t
    ; mutable pending_indents: int }

  let create =
    { buffer= Buffer.create 1024
    ; depth= 0
    ; size= 0
    ; wrapped= IntSet.empty
    ; pending_indents= 0 }

  let text t str width =
    t.size <- t.size + width ;
    Buffer.add_string t.buffer str

  let single_space t = text t " " 1

  let new_line t =
    t.size <- String.length indent * t.depth ;
    Buffer.add_char t.buffer '\n' ;
    if t.pending_indents > 0 then (
      t.size <- t.size + 2 ;
      t.depth <- t.depth + 1 ;
      t.pending_indents <- t.pending_indents - 1 ) ;
    for _ = 1 to t.depth do
      Buffer.add_string t.buffer indent
    done

  let rec node_gen t wrap = function
    | Nodes nodes ->
        List.iter (fun node -> node_gen t wrap node) nodes
    | Group (id, nodes) ->
        let width =
          List.fold_left (fun acc node -> acc + width t.wrapped node) 0 nodes
        in
        let wrap =
          match wrap with
          | Wrap.Disable ->
              Wrap.Disable
          | w when w = Wrap.Force || t.size + width > limit ->
              t.wrapped <- IntSet.add id t.wrapped ;
              Wrap.Enable
          | Wrap.Detect ->
              Wrap.Detect
          | Wrap.Enable ->
              Wrap.Detect
          | Wrap.Force ->
              Wrap.Detect
        in
        List.iter (fun node -> node_gen t wrap node) nodes
    | Fill nodes ->
        let wrap = ref wrap in
        let rec aux = function
          | [] ->
              ()
          | hd :: tl ->
              if hd = SpaceOrLine then
                let width =
                  try List.hd tl |> width t.wrapped with Failure _ -> 0
                in
                if t.size + width > limit then (
                  if !wrap = Wrap.Detect then wrap := Wrap.Enable ;
                  new_line t )
                else single_space t
              else node_gen t !wrap hd ;
              aux tl
        in
        aux nodes
    | Text (str, width) ->
        text t str width
    | Line when wrap = Wrap.Enable ->
        new_line t
    | HardLine ->
        new_line t
    | EmptyLine ->
        Buffer.add_char t.buffer '\n' ;
        new_line t
    | SpaceOrLine when wrap = Wrap.Enable ->
        new_line t
    | SpaceOrLine ->
        single_space t
    | Indent nodes when wrap = Wrap.Enable ->
        t.size <- t.size + String.length indent ;
        t.depth <- t.depth + 1 ;
        Buffer.add_string t.buffer indent ;
        List.iter (fun node -> node_gen t wrap node) nodes ;
        t.depth <- t.depth - 1
    | Indent nodes ->
        List.iter (fun node -> node_gen t wrap node) nodes
    | IndentNext nodes when wrap = Wrap.Enable ->
        t.pending_indents <- t.pending_indents + 1 ;
        let before = t.pending_indents in
        List.iter (fun node -> node_gen t wrap node) nodes ;
        if t.pending_indents = before then
          t.pending_indents <- t.pending_indents - 1
        else t.depth <- t.depth - 1
    | IndentNext nodes ->
        List.iter (fun node -> node_gen t wrap node) nodes
    | Line ->
        ()

  let generate t node =
    node_gen t Wrap.Detect node ;
    Buffer.contents t.buffer
end

module Builder = struct
  open Lys.Ast
  open Node

  type t = {mutable id: int}

  let create = {id= 0}

  let new_id (t : t) =
    t.id <- t.id + 1 ;
    t.id

  let spaced nodes = if nodes = [] then [] else SpaceOrLine :: nodes

  let separated_nodes sep nodes =
    List.mapi
      (fun i node ->
        if i = List.length nodes - 1 then node else Node.Nodes [node; sep] )
      nodes

  let delimited_nodes t left right nodes =
    Group (new_id t, [left; Line; Indent nodes; Line; right])

  let operator left right op =
    Fill [left; SpaceOrLine; Fill [op; SpaceOrLine; right]]

  let escape str = String.split_on_char '"' str |> String.concat "\\\""

  let quoted_string t str =
    Group (new_id t, [text "\""; text (escape str); text "\""])

  let boolean_string b = if b then text "True" else text "False"

  let rec build_program t decls =
    Nodes
      ( separated_nodes EmptyLine (List.map (fun d -> build_decl t d) decls)
      @ [HardLine] )

  and build_decl t decl =
    match decl.value with
    | Decl {id; params; signature; body} ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; build_olid id
          ; Nodes
              (spaced
                 (separated_nodes SpaceOrLine
                    (List.map (fun a -> build_param t a) params) ) )
          ; build_ann signature
          ; SpaceOrLine
          ; text "="
          ; Group (new_id t, [SpaceOrLine; Indent [build_expr t body]]) ]
    | Decls _ ->
        assert false
    | UnionDecl {id; polys; variants} ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id.value
          ; SpaceOrLine
          ; text ":="
          ; Nodes
              (spaced
                 (separated_nodes SpaceOrLine
                    (List.map (fun poly -> text poly.value) polys) ) )
          ; SpaceOrLine
          ; text "{"
          ; Group
              ( new_id t
              , spaced
                  (List.map
                     (fun v -> Nodes [Indent [build_variant t v]; HardLine])
                     variants ) )
          ; text "}" ]
    | SynDecl {id; typing} ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id.value
          ; SpaceOrLine
          ; text ":="
          ; Group (new_id t, [SpaceOrLine; Indent [build_typing typing]]) ]
    | Comment s ->
        Group
          ( new_id t
          , [ text "`"
            ; (if String.contains s '\n' then HardLine else SpaceOrLine)
            ; Indent [text (String.trim s)]
            ; (if String.contains s '\n' then HardLine else SpaceOrLine)
            ; text "`" ] )

  and build_olid olid =
    match olid.value with Wildcard -> text "_" | L str -> text str

  and build_param t param =
    match param.value with
    | PRLID str ->
        text str.value
    | PRTuple tuple_param ->
        build_tuple_param t tuple_param

  and build_ann ann =
    match ann with
    | None ->
        text ""
    | Some typing ->
        Nodes [SpaceOrLine; text ":"; SpaceOrLine; build_typing typing]

  and build_tuple_param t tuple_param =
    delimited_nodes t (text "(") (text ")")
      (separated_nodes
         (Nodes [text ","; SpaceOrLine])
         (List.map (fun a -> build_param t a) tuple_param) )

  and build_expr t expr =
    match expr.value with
    | EParenthesized e ->
        delimited_nodes t (text "(") (text ")") [build_expr t e]
    | ETyped {body; signature} ->
        Nodes [build_expr t body; build_ann signature]
    | EBoolOp {l; op; r} ->
        operator (build_expr t l) (build_expr t r) (build_bool_op op)
    | ECompOp {l; op; r} ->
        operator (build_expr t l) (build_expr t r) (build_comp_op op)
    | EPipeOp {l; r} ->
        operator (build_expr t l) (build_expr t r) (text "|>")
    | EConcatOp {l; r} ->
        operator (build_expr t l) (build_expr t r) (text "++")
    | EAddOp {l; op; r} ->
        operator (build_expr t l) (build_expr t r) (build_add_op op)
    | EMulOp {l; op; r} ->
        operator (build_expr t l) (build_expr t r) (build_mul_op op)
    | EUnOp {op; body} ->
        Fill [build_un_op op; build_expr t body]
    | EExpOp {l; r} ->
        operator (build_expr t l) (build_expr t r) (text "**")
    | EBitOp {l; op; r} ->
        operator (build_expr t l) (build_expr t r) (build_bit_op op)
    | EApp {fn; arg} ->
        Fill [build_expr t fn; SpaceOrLine; build_expr t arg]
    | ELambda {params; body} ->
        Nodes
          [ text "\\"
          ; Nodes
              (separated_nodes SpaceOrLine
                 (List.map (fun a -> build_param t a) params) )
          ; SpaceOrLine
          ; text "->"
          ; Group (new_id t, [SpaceOrLine; Indent [build_expr t body]]) ]
    | EMatch {ref; cases} ->
        Nodes
          [ Fill
              [ text "match"
              ; SpaceOrLine
              ; build_expr t ref
              ; SpaceOrLine
              ; text "{" ]
          ; Group
              ( new_id t
              , spaced
                  (List.map
                     (fun c -> Nodes [Indent [build_case t c]; SpaceOrLine])
                     cases ) )
          ; text "}" ]
    | ELets {binds; body} ->
        Nodes
          [ text "let"
          ; Group
              ( new_id t
              , [ IndentNext
                    (spaced
                       (separated_nodes
                          (Nodes [text ";"; SpaceOrLine])
                          (List.map (fun b -> build_bind t b) binds) ) ) ] )
          ; SpaceOrLine
          ; text "in"
          ; Group (new_id t, [SpaceOrLine; Indent [build_expr t body]]) ]
    | ELet _ ->
        assert false
    | EIf {predicate; truthy; falsy} ->
        Group
          ( new_id t
          , [ text "if"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expr t predicate]])
            ; SpaceOrLine
            ; text "then"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expr t truthy]])
            ; SpaceOrLine
            ; text "else"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expr t falsy]]) ] )
    | EUID str | ELID str ->
        text str.value
    | ETuple exprs ->
        delimited_nodes t (text "(") (text ")")
          (separated_nodes
             (Nodes [text ","; SpaceOrLine])
             (List.map (fun e -> build_expr t e) exprs) )
    | EList exprs ->
        delimited_nodes t (text "[") (text "]")
          (separated_nodes
             (Nodes [text ","; SpaceOrLine])
             (List.map (fun e -> build_expr t e) exprs) )
    | EUnit ->
        text "()"
    | EBool b ->
        boolean_string b.value
    | EString str ->
        quoted_string t str.value
    | EFloat str | EInt str ->
        text (String.lowercase_ascii str.value)

  and build_bool_op op =
    match op.value with OpBoolAnd -> text "&&" | OpBoolOr -> text "||"

  and build_comp_op op =
    match op.value with
    | OpGt ->
        text ">"
    | OpGeq ->
        text ">="
    | OpLt ->
        text "<"
    | OpLeq ->
        text "<="
    | OpEq ->
        text "=="
    | OpFeq ->
        text "~~"
    | OpNeq ->
        text "!="
    | OpNFeq ->
        text "!~"

  and build_add_op op =
    match op.value with OpAdd -> text "+" | OpSub -> text "-"

  and build_mul_op op =
    match op.value with
    | OpMul ->
        text "*"
    | OpDiv ->
        text "/"
    | OpMod ->
        text "%"

  and build_un_op op =
    match op.value with
    | UnPlus ->
        text "+"
    | UnNeg ->
        text "-"
    | UnBoolNot ->
        text "!"

  and build_bit_op op =
    match op.value with
    | OpBitLShift ->
        text "<<"
    | OpBitRShift ->
        text ">>"
    | OpBitAnd ->
        text "&"
    | OpBitOr ->
        text "|"
    | OpBitXor ->
        text "^"

  and build_bind t bind =
    Group
      ( new_id t
      , [ text bind.value.id.value
        ; Nodes
            (spaced
               (separated_nodes SpaceOrLine
                  (List.map (fun a -> build_param t a) bind.value.params) ) )
        ; build_ann bind.value.signature
        ; SpaceOrLine
        ; text "="
        ; Group (new_id t, [SpaceOrLine; Indent [build_expr t bind.value.body]])
        ] )

  and build_case t case =
    match case.value with
    | Case {pat; body} ->
        Group
          ( new_id t
          , [ build_pat t pat
            ; SpaceOrLine
            ; text "->"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expr t body]])
            ; text ";" ] )
    | CaseGuard {pat; guard; body} ->
        Group
          ( new_id t
          , [ build_pat t pat
            ; SpaceOrLine
            ; text "if"
            ; SpaceOrLine
            ; build_expr t guard
            ; SpaceOrLine
            ; text "->"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expr t body]])
            ; text ";" ] )

  and build_pat t pat =
    match pat.value with
    | PInt str | PFloat str ->
        text str.value
    | PString str ->
        quoted_string t str.value
    | PBool b ->
        boolean_string b.value
    | POLID str | PTail str ->
        build_olid str
    | PConstructor {id; params} ->
        Nodes
          [ text id.value
          ; Nodes
              (spaced
                 (separated_nodes SpaceOrLine
                    (List.map (fun p -> build_pat t p) params) ) ) ]
    | PList list_pat ->
        build_list_pat t list_pat
    | PListSpd list_spd_pat ->
        build_list_spd_pat t list_spd_pat
    | PTuple tuple_pat ->
        build_tuple_pat t tuple_pat
    | POr {l; r} ->
        Fill [build_pat t l; text ";"; SpaceOrLine; build_pat t r]
    | PParenthesized pat ->
        delimited_nodes t (text "(") (text ")") [build_pat t pat]

  and build_list_pat t list_pat =
    delimited_nodes t (text "[") (text "]")
      (separated_nodes
         (Nodes [text ","; SpaceOrLine])
         (List.map (fun p -> build_pat t p) list_pat.value) )

  and build_list_spd_pat t list_spd_pat =
    delimited_nodes t (text "[") (text "...]")
      (separated_nodes
         (Nodes [text ","; SpaceOrLine])
         (List.map (fun p -> build_pat t p) list_spd_pat.value) )

  and build_tuple_pat t tuple_pat =
    delimited_nodes t (text "(") (text ")")
      (separated_nodes
         (Nodes [text ","; SpaceOrLine])
         (List.map (fun p -> build_pat t p) tuple_pat.value) )

  and build_variant t variant =
    Group
      ( new_id t
      , [ text variant.value.id.value
        ; Fill
            (spaced
               (separated_nodes SpaceOrLine
                  (List.map build_typing variant.value.typings) ) )
        ; text ";" ] )

  and build_typing typing =
    match typing.value with
    | TInt ->
        text "Int"
    | TFloat ->
        text "Float"
    | TString ->
        text "String"
    | TBool ->
        text "Bool"
    | TUnit ->
        text "Unit"
    | TList typing ->
        Nodes [text "["; build_typing typing; text "]"]
    | TTuple typings ->
        Nodes
          [ text "("
          ; Nodes
              (separated_nodes
                 (Nodes [text ","; SpaceOrLine])
                 (List.map build_typing typings) )
          ; text ")" ]
    | TFunc {l; r} ->
        Nodes
          [build_typing l; SpaceOrLine; text "->"; SpaceOrLine; build_typing r]
    | TPoly str ->
        text str.value
    | TConstructor {id; typings} ->
        Nodes
          [ text id.value
          ; Nodes
              (spaced
                 (separated_nodes SpaceOrLine (List.map build_typing typings)) )
          ]
    | TTyping typing ->
        Nodes [text "("; build_typing typing; text ")"]
end

let format program =
  let build = Builder.create in
  let node = Builder.build_program build program in
  let gen = Generator.create in
  Generator.generate gen node
