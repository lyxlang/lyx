(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lyx

let parse ?(json = false) buf =
  try Parser.parse buf with
  | Lexer.Lexing_error span ->
      let code = 1 and msg = "Unexpected character." in
      let report = Reporter.create_report Reporter.Error code msg span in
      Reporter.print_reports ~json [report] ;
      exit code
  | Parser.Syntax_error (code, msg, span) ->
      let code = code + 2 in
      let report = Reporter.create_report Reporter.Error code msg span in
      Reporter.print_reports ~json [report] ;
      exit code

let analyze ?(json = false) program =
  try Usage_analysis.analyze_program ~json program
  with Usage_analysis.Output.Analysis_error reports ->
    Reporter.print_reports ~json reports ;
    exit (if json then 0 else 1000)

let transpile_file file =
  let buf = Sedlexing.Utf8.from_channel (open_in_bin file) in
  Sedlexing.set_filename buf file ;
  buf |> parse |> Desugar.desugar_program |> analyze |> Transpiler.build_program
  |> Printf.fprintf (open_out_bin (file ^ ".ml")) "%s%!"

let transpile_stdin () =
  Sedlexing.Utf8.from_channel stdin
  |> parse |> Desugar.desugar_program |> analyze |> Transpiler.build_program
  |> print_string ;
  flush stdout

let format_file file =
  Sedlexing.Utf8.from_channel (open_in_bin file)
  |> parse |> Formatter.format
  |> Printf.fprintf (open_out_bin file) "%s%!"

let format_stdin () =
  Sedlexing.Utf8.from_channel stdin |> parse |> Formatter.format |> print_string ;
  flush stdout

let fmt_debug () =
  let ast = Sedlexing.Utf8.from_channel stdin |> parse ~json:true in
  ast |> Formatter.format |> print_string ;
  flush stdout ;
  ast |> Desugar.desugar_program |> analyze ~json:true |> ignore

let parse_file file =
  let buf = Sedlexing.Utf8.from_channel (open_in_bin file) in
  Sedlexing.set_filename buf file ;
  buf |> parse |> Desugar.desugar_program |> analyze |> Ast.show_program
  |> print_endline

let parse_stdin () =
  Sedlexing.Utf8.from_channel stdin
  |> parse |> Desugar.desugar_program |> analyze |> Ast.show_program
  |> print_endline

let print_usage () =
  print_endline
    {|╭─╴Error╶──────────────────────────────────────────────────────────────────╮
│ Invalid arguments.                                                       │
╰──────────────────────────────────────────────────────────────────────────╯

╭──────────────────────┬───────────────────────────────────────────────────╮
│ Command              │ Description                                       │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lyx                  │ Parse standard input and display the AST.         │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lyx <file>           │ Parse a file and display the AST.                 │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lyx fmt              │ Format standard input and output it.              │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lyx fmt <file>       │ Format a file and overwrite it.                   │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lyx transpile        │ Transpile standard input and output it.           │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lyx transpile <file> │ Transpile a file and write its OCaml counterpart. │
╰──────────────────────┴───────────────────────────────────────────────────╯|} ;
  exit 1

let () =
  match Sys.argv with
  | [|_; "transpile"; file|] ->
      transpile_file file
  | [|_; "transpile"|] ->
      transpile_stdin ()
  | [|_; "fmt"; file|] ->
      format_file file
  | [|_; "fmt"|] ->
      format_stdin ()
  | [|_; "fmt-debug"|] ->
      fmt_debug ()
  | [|_; file|] ->
      parse_file file
  | [|_|] ->
      parse_stdin ()
  | _ ->
      print_usage ()
