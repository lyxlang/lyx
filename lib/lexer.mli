(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

(** Exception raised when a lexing error is encountered during tokenization.
    Contains the positions where the error occurred in the source text.

    @param start The position where the lexical error begins.
    @param fin The position where the lexical error ends. *)
exception Lexing_error of Lexing.position * Lexing.position

val lexer :
     Sedlexing.lexbuf
  -> unit
  -> Menhir_parser.token * Lexing.position * Lexing.position
(** Creates a lexer function for tokenizing Lyx source code. This lexer converts
    the input text into a stream of tokens for the parser. It handles keywords,
    operators, literals, identifiers, and comments.

    @param buf The UTF-8 buffer containing the source text to tokenize.
    @return A function that produces tokens along with their source positions.
    @raise Lexing_error When an invalid token is encountered. *)
