(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type uid = string

and lid = string

and program = declaration list

and declaration =
  | Comment of string
  | ValueBinding of binding
  | TypeDefinition of {id: uid; body: typing}
  | FunctionDefinition of
      { id: lid
      ; parameters: parameter list
      ; signature: signature
      ; body: expression }
  | AdtDefinition of {id: uid; polymorphics: lid list; variants: variant list}

and binding = {id: lid; signature: signature; body: expression}

and signature = typing option

and parameter = ALid of lid | ATuple of parameter list

and typing =
  | TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TConstructor of {id: uid; typing: typing option}
  | TPolymorphic of lid
  | TTuple of typing list
  | TList of typing
  | TFunction of {l: typing; r: typing}

and expression =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Unit
  | Uid of uid
  | Lid of lid
  | Tuple of expression list
  | List of expression list
  | BinaryOperation of {l: expression; operator: binary_operator; r: expression}
  | UnaryOperation of {operator: unary_operator; body: expression}
  | Let of {bindings: binding list; body: expression}
  | If of {predicate: expression; truthy: expression; falsy: expression}
  | Match of {body: expression; cases: case list}
  | Lambda of {parameters: parameter list; body: expression}
  | Application of {body: expression; argument: expression}
  | Expression of {body: expression; signature: signature}

and binary_operator =
  | BPipe
  | BOr
  | BAnd
  | BEqual
  | BNotEqual
  | BGreaterThan
  | BGreaterOrEqual
  | BLessThan
  | BLessOrEqual
  | BConcatenate
  | BAdd
  | BSubstract
  | BMultiply
  | BDivide
  | BModulo
  | BExponentiate

and unary_operator = UPlus | UMinus | UNot

and variant = {id: uid; typing: typing option}

and case = {pattern: pattern; guard: expression option; body: expression}

and pattern =
  | PInt of int
  | PFloat of float
  | PBool of bool
  | PString of string
  | PLid of lid
  | PTuple of pattern list
  | PList of pattern list
  | PListSpread of pattern list
  | PConstructor of {id: uid; pattern: pattern option}
  | POr of {l: pattern; r: pattern}
[@@deriving show {with_path= false}]
