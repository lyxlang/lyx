<!--
SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# The Lyx pipeline: from source code to executable program

In this guide, we’ll walk through the complete pipeline that transforms high-level source code into a runnable program. We’ll cover each stage — from lexing and parsing to analysis and transpiling — explaining their roles and how they interconnect.

## 1. Lexing: transforming text into tokens

The first stage in the pipeline is lexing. Think of this as the language’s “eye” that sees raw text and turns it into meaningful symbols. In Lyx, the lexer is written in OCaml using the [sedlex](https://github.com/ocaml-community/sedlex) library.

The raw source code file, like our [`grammar.lyx`](https://github.com/lyxlang/lyx/blob/main/examples/grammar.lyx) example, is fed as input to the lexer.

The lexer scans the text character by character using regular expressions to identify important elements such as keywords (`def`, `if`, `else`), operators (`+`, `-`, `**`), literals (numbers, strings), and identifiers.

The lexer outputs tokens such as `KWDEF`, `UID`, `LID`, etc. For instance, encountering `"def"` in the source code produces the token `KWDEF`.

Invalid sequences throw lexing errors with position information, ensuring that you know exactly where the problem lies.

By breaking down the code text into tokens, the lexer provides the raw ingredients for the parser to work with.

## 2. Parsing: building the Abstract Syntax Tree

Once lexed, the stream of tokens is fed into the parser. The parser organizes these tokens into a structured format known as an Abstract Syntax Tree (AST). For Lyx, the parser specification is written using Menhir, an OCaml parser generator.

The file [`menhir_parser.mly`](https://github.com/lyxlang/lyx/blob/main/lib/menhir_parser.mly) defines the grammar. Here, production rules describe how tokens combine to form constructs like declarations, expressions, function parameters, and type annotations.

Each parsing rule constructs a node in the AST. For example, a function declaration or a pattern match is represented by a corresponding AST node (like `Decl` or `EMatch`).

The resulting AST captures the hierarchical structure of your program. It distinguishes between different language constructs while preserving critical position information (useful for error messages later in the pipeline).

The parser’s job is to ensure that your program is syntactically correct, providing a reliable intermediate representation for further stages.

## 3. Semantic analysis: adding meaning with usage analysis and type checking

Even if the code is syntactically sound, it still needs to make sense logically. This is where analysis comes in. Lyx performs several kinds of analysis to check the program’s logic and enforce language rules.

The AST first needs to be simplified, this is where desugaring comes in, see [`desugar.ml`](https://github.com/lyxlang/lyx/blob/main/lib/desugar.ml). It walks the AST and simplify the sugared syntax: for example, it transform lambdas of multiple parameters to multiple single-parameter lambdas chained together. In doing so, the AST becomes much easier for the coming analysis passes to handle.

Next up comes usage analysis, see [`usage_analysis.ml`](https://github.com/lyxlang/lyx/blob/main/lib/usage_analysis.ml). It again walks the AST, and for now only flags undefined identifiers as errors.

## 4. Transpiling: converting the AST to another language

With a fully analyzed and semantically sound AST in place, Lyx now needs to transform it into a target language, in our case OCaml. The transpiler does this transformation.

The transpiler (see [`transpiler.ml`](https://github.com/lyxlang/lyx/blob/main/bin/transpiler.ml)) works with the validated AST nodes (produced from the parsing and analysis stages) as input.

It walks over the AST and outputs equivalent OCaml code. For example, Lyx’s `+` and `-` might be transpiled to OCaml’s `+.`, `-.` for floating-point operations.

For features that have no direct OCaml counterpart, the transpiler inserts pre-written OCaml functions to achieve the same functionality.

It outputs a neatly formatted OCaml source file that is then compiled by the OCaml compiler to produce the final executable.

## Wrapping it all up: the complete pipeline

To summarize, here’s how the Lyx pipeline transforms your code:

1. Lexing:

   - Reads source files as text.
   - Breaks the text into a stream of tokens using the [sedlex](https://github.com/ocaml-community/sedlex) library.

2. Parsing:

   - Uses Menhir-defined grammar to convert tokens into an AST.
   - Constructs a structured representation capturing all language constructs.

3. Analysis:

   - The AST is desugared to make it easier to work with.
   - Undefined identifiers are flagged as errors using usage analysis.

4. Transpiling:

   - Walks over the finalized AST.
   - Outputs equivalent OCaml code, ready for compilation.

Each phase is a building block that meticulously checks and transforms the program. This design not only makes the compiler robust against errors but also allows each stage to be maintained and improved independently.
