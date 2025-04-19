(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
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
    | Empty

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
    | Line | Empty ->
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
    { buffer= Buffer.create 4096
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
        List.iter (node_gen t wrap) nodes
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
          | Wrap.Detect | Wrap.Enable | Wrap.Force ->
              Wrap.Detect
        in
        List.iter (node_gen t wrap) nodes
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
        List.iter (node_gen t wrap) nodes ;
        t.depth <- t.depth - 1
    | Indent nodes ->
        List.iter (node_gen t wrap) nodes
    | IndentNext nodes when wrap = Wrap.Enable ->
        t.pending_indents <- t.pending_indents + 1 ;
        let before = t.pending_indents in
        List.iter (node_gen t wrap) nodes ;
        if t.pending_indents = before then
          t.pending_indents <- t.pending_indents - 1
        else t.depth <- t.depth - 1
    | IndentNext nodes ->
        List.iter (node_gen t wrap) nodes
    | Line | Empty ->
        ()

  let generate t node =
    node_gen t Wrap.Detect node ;
    Buffer.contents t.buffer
end

module Builder = struct
  open Lyx.Ast
  open Node

  type t = {mutable id: int}

  let create = {id= 0}

  let new_id (t : t) =
    t.id <- t.id + 1 ;
    t.id

  let spaced nodes = if nodes = [] then [] else SpaceOrLine :: nodes

  let separated_nodes sep nodes =
    let last = List.length nodes - 1 in
    List.mapi (fun i node -> if i = last then node else Nodes [node; sep]) nodes

  let delimited_nodes t left right nodes =
    Group (new_id t, [left; Line; Indent nodes; Line; right])

  let escape str = str |> String.split_on_char '"' |> String.concat "\\\""

  let quoted_string t str =
    Group (new_id t, [text "\""; text (escape str); text "\""])

  let boolean_string b = text (if b then "True" else "False")

  let rec build_program t program =
    program
    |> List.map (build_declaration t)
    |> separated_nodes EmptyLine
    |> fun nodes -> Nodes (nodes @ [HardLine])

  and build_declaration t = function
    | DComment (_, str) ->
        let content_node = Indent [text (String.trim str)] in
        let line_node =
          if String.contains str '\n' then HardLine else SpaceOrLine
        in
        Group
          (new_id t, [text "`"; line_node; content_node; line_node; text "`"])
    | DValueBinding (_, binding) ->
        Nodes [text "def"; SpaceOrLine; build_binding t binding]
    | DTypeDefinition (_, {id; body}) ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id
          ; SpaceOrLine
          ; text ":="
          ; Group (new_id t, [SpaceOrLine; Indent [build_typing t body]]) ]
    | DFunctionDefinition (_, {id; parameters; signature; body}) ->
        let param_nodes =
          parameters
          |> List.map (build_parameter t)
          |> separated_nodes SpaceOrLine
          |> spaced
        in
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id
          ; Nodes param_nodes
          ; build_signature t signature
          ; SpaceOrLine
          ; text "="
          ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]
    | DADTDefinition (_, {id; polymorphics; variants}) ->
        let poly_nodes =
          polymorphics |> List.map text |> separated_nodes SpaceOrLine |> spaced
        in
        let variant_nodes =
          variants
          |> List.map (fun v -> Nodes [Indent [build_variant t v]; HardLine])
          |> spaced
        in
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id
          ; SpaceOrLine
          ; text ":="
          ; Nodes poly_nodes
          ; SpaceOrLine
          ; text "{"
          ; Group (new_id t, variant_nodes)
          ; text "}" ]

  and build_binding t {id; signature; body; _} =
    Fill
      [ text id
      ; build_signature t signature
      ; SpaceOrLine
      ; text "="
      ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]

  and build_signature t = function
    | None ->
        Empty
    | Some typing ->
        Nodes [SpaceOrLine; text ":"; SpaceOrLine; build_typing t typing]

  and build_parameter t = function
    | ALID (_, lid) ->
        text lid
    | ATuple (_, [param]) ->
        Fill [text "("; build_parameter t param; text ",)"]
    | ATuple (_, params) ->
        params
        |> List.map (build_parameter t)
        |> separated_nodes (Fill [text ","; SpaceOrLine])
        |> delimited_nodes t (text "(") (text ")")

  and build_typing t = function
    | TInt _ ->
        text "Int"
    | TFloat _ ->
        text "Float"
    | TString _ ->
        text "String"
    | TBool _ ->
        text "Bool"
    | TUnit _ ->
        text "Unit"
    | TList (_, typing) ->
        Nodes [text "["; build_typing t typing; text "]"]
    | TTuple (_, [typing]) ->
        Nodes [text "("; build_typing t typing; text ",)"]
    | TTuple (_, typings) ->
        typings
        |> List.map (build_typing t)
        |> separated_nodes (Nodes [text ","; SpaceOrLine])
        |> delimited_nodes t (text "(") (text ")")
    | TFunction (_, {l; r}) ->
        Nodes
          [ build_typing t l
          ; SpaceOrLine
          ; text "->"
          ; SpaceOrLine
          ; build_typing t r ]
    | TPolymorphic (_, lid) ->
        text lid
    | TConstructor (_, {id; typing}) ->
        Nodes
          [ text id
          ; Option.fold ~none:Empty
              ~some:(fun ty -> Nodes [SpaceOrLine; build_typing t ty])
              typing ]

  and build_expression t = function
    | EInt (_, str) ->
        text str
    | EFloat (_, str) ->
        text str
    | EBool (_, b) ->
        boolean_string b
    | EString (_, str) ->
        quoted_string t str
    | EUnit _ ->
        text "()"
    | EConstructor (_, {id; body}) ->
        Nodes
          [ text id
          ; Option.fold ~none:Empty
              ~some:(fun b -> Nodes [SpaceOrLine; build_expression t b])
              body ]
    | ELID (_, lid) ->
        text lid
    | ETuple (_, [expr]) ->
        Nodes [text "("; build_expression t expr; text ",)"]
    | ETuple (_, exprs) ->
        exprs
        |> List.map (build_expression t)
        |> separated_nodes (Nodes [text ","; SpaceOrLine])
        |> delimited_nodes t (text "(") (text ")")
    | EList (_, exprs) ->
        exprs
        |> List.map (build_expression t)
        |> separated_nodes (Nodes [text ","; SpaceOrLine])
        |> delimited_nodes t (text "[") (text "]")
    | EBinaryOperation (_, {l; operator; r}) ->
        Fill
          [ build_expression t l
          ; SpaceOrLine
          ; build_binary_operator operator
          ; SpaceOrLine
          ; build_expression t r ]
    | EUnaryOperation (_, {operator; body}) ->
        Fill [build_unary_operator operator; build_expression t body]
    | ELet (_, {bindings; body}) ->
        let binding_nodes =
          bindings
          |> List.map (build_binding t)
          |> separated_nodes (Nodes [text ";"; SpaceOrLine])
          |> spaced
        in
        Nodes
          [ text "let"
          ; Group (new_id t, [IndentNext binding_nodes])
          ; SpaceOrLine
          ; text "in"
          ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]
    | EDesugaredLet _ ->
        failwith "The formatter expects sugared let."
    | EIf (_, {predicate; truthy; falsy}) ->
        Group
          ( new_id t
          , [ text "if"
            ; Group
                (new_id t, [SpaceOrLine; Indent [build_expression t predicate]])
            ; SpaceOrLine
            ; text "then"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expression t truthy]])
            ; SpaceOrLine
            ; text "else"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expression t falsy]])
            ] )
    | EMatch (_, {body; cases}) ->
        let case_nodes =
          cases
          |> List.map (fun c -> Nodes [Indent [build_case t c]; SpaceOrLine])
          |> spaced
        in
        Nodes
          [ Fill
              [ text "match"
              ; SpaceOrLine
              ; build_expression t body
              ; SpaceOrLine
              ; text "{" ]
          ; Group (new_id t, case_nodes)
          ; text "}" ]
    | ELambda (_, {parameters; body}) ->
        let param_nodes =
          parameters
          |> List.map (build_parameter t)
          |> separated_nodes (Nodes [text " "])
        in
        Fill
          [ text "\\"
          ; Fill param_nodes
          ; SpaceOrLine
          ; text "->"
          ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]
    | EDesugaredLambda _ ->
        failwith "The formatter expects sugared lambdas."
    | EApplication (_, {body; argument}) ->
        Fill [build_expression t body; SpaceOrLine; build_expression t argument]
    | EExpression (_, {body; signature}) ->
        Nodes
          [ text "("
          ; build_expression t body
          ; build_signature t signature
          ; text ")" ]

  and build_binary_operator = function
    | BPipe ->
        text "|>"
    | BOr ->
        text "||"
    | BAnd ->
        text "&&"
    | BEqual ->
        text "=="
    | BNotEqual ->
        text "!="
    | BGreaterThan ->
        text ">"
    | BGreaterOrEqual ->
        text ">="
    | BLessThan ->
        text "<"
    | BLessOrEqual ->
        text "<="
    | BConcatenate ->
        text "++"
    | BAdd ->
        text "+"
    | BSubstract ->
        text "-"
    | BMultiply ->
        text "*"
    | BDivide ->
        text "/"
    | BModulo ->
        text "%"
    | BExponentiate ->
        text "**"

  and build_unary_operator = function
    | UPlus ->
        text "+"
    | UMinus ->
        text "-"
    | UNot ->
        text "!"

  and build_case t {pattern; guard; body; _} =
    let build_guard = function
      | Some guard_expr ->
          Fill
            [SpaceOrLine; text "if"; SpaceOrLine; build_expression t guard_expr]
      | None ->
          Empty
    in
    Fill
      [ build_pattern t pattern
      ; build_guard guard
      ; SpaceOrLine
      ; text "->"
      ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]])
      ; text ";" ]

  and build_pattern t = function
    | PInt (_, str) ->
        text str
    | PFloat (_, str) ->
        text str
    | PString (_, str) ->
        quoted_string t str
    | PBool (_, b) ->
        boolean_string b
    | PLID (_, lid) ->
        text lid
    | PTuple (_, [pattern]) ->
        Nodes [text "("; build_pattern t pattern; text ",)"]
    | PTuple (_, patterns) ->
        patterns
        |> List.map (build_pattern t)
        |> separated_nodes (Nodes [text ","; SpaceOrLine])
        |> delimited_nodes t (text "(") (text ")")
    | PList (_, patterns) ->
        patterns
        |> List.map (build_pattern t)
        |> separated_nodes (Nodes [text ","; SpaceOrLine])
        |> delimited_nodes t (text "[") (text "]")
    | PListSpread (_, patterns) ->
        let regular_patterns = List.rev (List.tl (List.rev patterns)) in
        let spread_pattern = List.hd (List.rev patterns) in
        let pattern_nodes =
          regular_patterns
          |> List.map (build_pattern t)
          |> separated_nodes (Nodes [text ","; SpaceOrLine])
        in
        let spread_node =
          Nodes [SpaceOrLine; text "..."; build_pattern t spread_pattern]
        in
        delimited_nodes t (text "[") (text "]") (pattern_nodes @ [spread_node])
    | PConstructor (_, {id; pattern}) ->
        Fill
          [ text id
          ; Option.fold ~none:Empty
              ~some:(fun p -> Fill [SpaceOrLine; build_pattern t p])
              pattern ]
    | POr (_, {l; r}) ->
        Fill [build_pattern t l; text ";"; SpaceOrLine; build_pattern t r]

  and build_variant t {id; typing; _} =
    Group
      ( new_id t
      , [ text id
        ; Option.fold ~none:Empty
            ~some:(fun ty ->
              Nodes [SpaceOrLine; text "as"; SpaceOrLine; build_typing t ty] )
            typing
        ; text ";" ] )
end

let format program =
  program
  |> Builder.build_program Builder.create
  |> Generator.generate Generator.create
