(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

val desugar_program : Ast.declaration list -> Ast.declaration list
(** Transforms the AST to desugar complex expressions into simpler ones. This
    function converts high-level syntactic constructs into more primitive ones:
    - Function definitions are transformed into value bindings with lambdas.
    - Let expressions are desugared into a series of desugared lets.
    - Lambda expressions are transformed into a series of desugared lambdas.

    This process simplifies subsequent compilation phases by reducing the number
    of AST node types that need to be handled.

    @param program The original, sugared AST program.
    @return A desugared AST program with simpler expression forms. *)
