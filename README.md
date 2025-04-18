<!--
SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

<div align="center">
  <br />
  <picture>
    <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/lyxlang/.github/main/media/brand-dark.png">
    <img src="https://raw.githubusercontent.com/lyxlang/.github/main/media/brand-light.png" alt="Lyx" width="400">
  </picture>
  <p align="center">
    The Lyx purely functional programming language.
    <br />
    <a href="https://github.com/lyxlang/vscode-lyx">VS Code Extension</a> •
    <a href="#usage">Usage</a> •
    <a href="#installation">Installation</a> •
    <a href="#contributing">Contributing</a> •
    <a href="#license">License</a>
  </p>
</div>

## Features

- Purely functional
  - No side effects
  - Pattern matching
  - Polymorphism
- Practical syntax
  - Concise
  - Elegant
  - Seamless currying support
- VS Code language support
  - Syntax highlighting
  - Formatting
  - Parsing
- Helpful error messages
- Unicode support

## Examples

#### Summation

```lyx
def sum lst =
  match lst {
    [] -> 0;
    [hd, tl...] -> hd + sum tl;
  }
```

#### Fibonacci

```lyx
def fib n : Int -> Int =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)
```

#### Grammar

For a detailed presentation of the Lyx syntax and features, please have a look at [our sample grammar file](https://github.com/lyxlang/lyx/blob/main/examples/grammar.lyx).

## Usage

You can skip installation when [running from source](https://github.com/lyxlang/lyx/wiki/Running-from-source).

The expected file extension of the Lyx programming language is `.lyx`.

Following [docopt](http://docopt.org/) conventions:

| Command                | Description                                       |
| :--------------------- | :------------------------------------------------ |
| `lyx`                  | Parse standard input and display the AST.         |
| `lyx <file>`           | Parse a file and display the AST.                 |
| `lyx fmt`              | Format standard input and output it.              |
| `lyx fmt <file>`       | Format a file and overwrite it.                   |
| `lyx transpile`        | Transpile standard input and output it.           |
| `lyx transpile <file>` | Transpile a file and write its OCaml counterpart. |

You can run transpiled files using:

```sh
ocaml <file.ml>
```

## Installation

If you are using [NixOS](https://nixos.org/), please follow [these steps](https://github.com/lyxlang/lyx/wiki/Installation-on-NixOS).

Or, download [the latest executable](https://github.com/lyxlang/lyx/releases/latest) for your system and [add it to your PATH](https://github.com/lyxlang/lyx/wiki/Adding-Lyx-to-the-PATH).

On Unix-like operating systems, you will then need to run the following command:

```sh
chmod +x /path/to/lyx/executable
```

## Contributing

We welcome contributions to Lyx! If you’re interested in helping improve the language, fix bugs, or add new features, please check out our [contributing guide](https://github.com/lyxlang/lyx/blob/main/docs/CONTRIBUTING.md) for detailed information on how to get started, set up your development environment, and submit your contributions.

Key areas where you can contribute include:

- Improving the back-end
- Enhancing language features
- Writing documentation
- Creating examples and tutorials
- Reporting and fixing bugs
- Suggesting and implementing new features

Whether you’re a seasoned OCaml developer or just getting started with functional programming, there are many ways to contribute. We appreciate all forms of contribution and look forward to collaborating with you!

## License

This project is licensed under multiple licenses in accordance with the recommendations of the [REUSE Initiative](https://reuse.software/). Please refer to the individual files for more information.
