(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lyx

let parse ?(json = false) buf =
  try Parser.parse buf with
  | Lexer.Lexing_error (start, fin) ->
      let code = 1 and msg = "Unexpected character." in
      Reporter.create_report Reporter.Error code msg (start, fin) ~json ;
      exit code
  | Parser.Syntax_error (code, msg, start, fin) ->
      let code = code + 2 in
      Reporter.create_report Reporter.Error code msg (start, fin) ~json ;
      exit code

let analyze ?(json = false) program =
  try Usage_analysis.analyze_program program
  with Usage_analysis.Output.Analysis_error errors ->
    List.iter
      (fun e ->
        Reporter.create_report Reporter.Error
          (Usage_analysis.Output.code_of_error e)
          (Usage_analysis.Output.string_of_error e)
          (Usage_analysis.Output.span_of_error e)
          ~json )
      errors ;
    exit 1000

let transpile_file file =
  Sedlexing.Utf8.from_channel (open_in_bin file)
  |> parse |> Desugar.desugar_program |> analyze |> Transpiler.build_program
  |> Printf.fprintf (open_out_bin (file ^ ".ml")) "%s%!"

let transpile_stdin () =
  Sedlexing.Utf8.from_channel stdin
  |> parse |> Desugar.desugar_program |> analyze |> Transpiler.build_program
  |> print_string ;
  flush stdout

let format_file file =
  Sedlexing.Utf8.from_channel (open_in_bin file)
  |> parse ~json:true |> Formatter.format
  |> Printf.fprintf (open_out_bin file) "%s%!"

let format_stdin () =
  Sedlexing.Utf8.from_channel stdin
  |> parse ~json:true |> Formatter.format |> print_string ;
  flush stdout

let parse_file file =
  let buf = Sedlexing.Utf8.from_channel (open_in_bin file) in
  Sedlexing.set_filename buf file ;
  let ast = parse buf |> Desugar.desugar_program |> analyze in
  Ast.show_program ast |> print_endline

let parse_stdin () =
  let ast =
    Sedlexing.Utf8.from_channel stdin
    |> parse |> Desugar.desugar_program |> analyze
  in
  Ast.show_program ast |> print_endline

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
  | [|_; file|] ->
      parse_file file
  | [|_|] ->
      parse_stdin ()
  | _ ->
      print_usage ()
