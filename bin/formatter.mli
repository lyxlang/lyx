(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

val format : Lys.Ast.program -> string
(** Formats a Lys AST into beautifully-formatted source code with proper
    indentation and line wrapping. The formatter implements a pretty-printing
    algorithm that respects an 80-character line length limit.

    The AST must not be desugared before formatting. This function expects
    syntax sugar constructs like let expressions, lambdas, and function
    definitions to be present in their original form. Using a desugared AST will
    cause the formatter to fail with an error.

    @param program
      The AST representing a Lys program in its original, non-desugared form.
    @return A string containing the formatted Lys source code.
    @raise Failure When attempting to format a desugared AST. *)
