(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type program = Ast.decl list

val pp_program : Format.formatter -> program -> unit

val show_program : program -> string

val analyze : Ast.program -> Ast.decl list
