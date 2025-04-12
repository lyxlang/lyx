(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Sedlexing.Utf8
open Menhir_parser

exception Lexing_error of Lexing.position * Lexing.position

let digit = [%sedlex.regexp? '0' .. '9']

let exp = [%sedlex.regexp? 'e' | 'E', Opt ('+' | '-'), Plus digit]

let integer = [%sedlex.regexp? Plus digit, Opt exp]

let floating = [%sedlex.regexp? Plus digit, '.', Plus digit, Opt exp]

let alnum = [%sedlex.regexp? lowercase | uppercase | digit]

let uid = [%sedlex.regexp? uppercase, Star alnum, Star '\'']

let lid = [%sedlex.regexp? lowercase, Star alnum, Star '\'']

let string buf =
  let buffer = Buffer.create 8 in
  let rec aux buf =
    match%sedlex buf with
    | {|\"|} ->
        Buffer.add_char buffer '"' ; aux buf
    | Compl '"' ->
        Buffer.add_string buffer (lexeme buf) ;
        aux buf
    | '"' ->
        STRING (Buffer.contents buffer)
    | _ ->
        assert false
  in
  aux buf

let comment buf =
  let buffer = Buffer.create 16 in
  let rec aux buf =
    match%sedlex buf with
    | Compl '`' ->
        Buffer.add_string buffer (lexeme buf) ;
        aux buf
    | '`' ->
        COMMENT (Buffer.contents buffer)
    | _ ->
        assert false
  in
  aux buf

let rec tokenizer buf =
  match%sedlex buf with
  | white_space ->
      tokenizer buf
  | "def" ->
      KWDEF
  | "True" ->
      BOOL true
  | "False" ->
      BOOL false
  | "if" ->
      KWIF
  | "then" ->
      KWTHEN
  | "else" ->
      KWELSE
  | "let" ->
      KWLET
  | "in" ->
      KWIN
  | "match" ->
      KWMATCH
  | "Int" ->
      KWINT
  | "Float" ->
      KWFLOAT
  | "String" ->
      KWSTRING
  | "Bool" ->
      KWBOOL
  | "Unit" ->
      KWUNIT
  | '=' ->
      EQUAL
  | ':' ->
      COLON
  | ":=" ->
      COLONEQUAL
  | '_' ->
      UNDERSCORE
  | ',' ->
      COMMA
  | "..." ->
      ELLIPSIS
  | '[' ->
      LBRACKET
  | ']' ->
      RBRACKET
  | '(' ->
      LPAREN
  | ')' ->
      RPAREN
  | '{' ->
      LBRACE
  | '}' ->
      RBRACE
  | "|>" ->
      TRIANGLE
  | "++" ->
      PLUSPLUS
  | "**" ->
      STARSTAR
  | "()" ->
      UNIT
  | "&&" ->
      ANDAND
  | "||" ->
      BARBAR
  | '>' ->
      GT
  | ">=" ->
      GEQ
  | '<' ->
      LT
  | "<=" ->
      LEQ
  | "==" ->
      EQUALEQUAL
  | "~~" ->
      TILDETILDE
  | "!=" ->
      BANGEQUAL
  | "!~" ->
      BANGTILDE
  | '+' ->
      PLUS
  | '-' ->
      MINUS
  | '*' ->
      STAR
  | '/' ->
      SLASH
  | '%' ->
      PERCENT
  | '!' ->
      BANG
  | "->" ->
      ARROW
  | ';' ->
      SEMICOLON
  | '\\' ->
      BACKSLASH
  | integer ->
      INT (lexeme buf)
  | floating ->
      FLOAT (lexeme buf)
  | '"' ->
      string buf
  | '`' ->
      comment buf
  | uid ->
      UID (lexeme buf)
  | lid ->
      LID (lexeme buf)
  | eof ->
      EOF
  | _ ->
      let start, fin = Sedlexing.lexing_positions buf in
      raise @@ Lexing_error (start, fin)

let lexer buf = Sedlexing.with_tokenizer tokenizer buf
