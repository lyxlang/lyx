(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type arity = int

type info = Ast.span * arity

type name = string

type map = (name, info) Hashtbl.t

type error =
  | AlreadyDefined of {prev: Ast.span; new': string; newest_span: Ast.span}
  | Undefined of {name: string; span: Ast.span}
  | ReservedName of {name: string; span: Ast.span}
  | NotACallee of {name: string; span: Ast.span}
  | ArityMismatch of {name: string; expected: int; span: Ast.span}

val pp_error :
  Ppx_deriving_runtime.Format.formatter -> error -> Ppx_deriving_runtime.unit

val show_error : error -> Ppx_deriving_runtime.string

type warning = Unused of {name: string; span: Ast.span}

val pp_warning :
  Ppx_deriving_runtime.Format.formatter -> warning -> Ppx_deriving_runtime.unit

val show_warning : warning -> Ppx_deriving_runtime.string

type analysis_output =
  { mutable errors: error list
  ; mutable warnings: (name, warning) Hashtbl.t
  ; mutable types: map
  ; mutable variants: map }

val analyze_program : Ast.program -> analysis_output

val debug_output : analysis_output -> unit

val get_errors : analysis_output -> error list
