(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type program = Ast.decl list

val pp_program :
  Ppx_deriving_runtime.Format.formatter -> program -> Ppx_deriving_runtime.unit

val show_program : program -> Ppx_deriving_runtime.string

val analyze : Ast.program -> Ast.decl list

val debug_output : Ast.decl list -> unit
