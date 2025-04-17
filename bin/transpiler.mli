(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

val build_program : Lys.Ast.program -> string
(** Translates a Lys AST into OCaml source code.

    This function transpiles Lys language constructs directly to their OCaml
    equivalents. It handles type definitions, value bindings, pattern matching,
    algebraic data types, and all expressions supported by the Lys language.

    The AST must be desugared before transpiling. This function expects complex
    constructs like function definitions and let expressions to be transformed
    into their simpler forms (value bindings with lambdas and desugared lets).
    The transpiler will fail with an error message if it encounters
    non-desugared nodes.

    Built-in functions and operators are mapped to their OCaml counterparts
    (e.g., “printString” becomes “print_endline”, “π” becomes “Float.pi”).

    @param program The desugared AST representing a Lys program.
    @return OCaml source code as a string ready for compilation.
    @raise Failure
      When encountering non-desugared constructs like function definitions or
      standard let expressions. *)
