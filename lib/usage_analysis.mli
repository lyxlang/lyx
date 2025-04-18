(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

module Output : sig
  type error = Undefined of {name: string; span: Ast.span}

  exception Analysis_error of error list

  val code_of_error : error -> int

  val string_of_error : error -> string

  val span_of_error : error -> Ast.span
end

val analyze_program : Ast.declaration list -> Ast.declaration list
