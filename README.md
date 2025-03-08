<!--
SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

<div align="center">
  <br />
  <picture>
    <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/lyslang/.github/main/media/brand-dark.png">
    <img src="https://raw.githubusercontent.com/lyslang/.github/main/media/brand-light.png" alt="Lys" width="400">
  </picture>
  <p align="center">
    <br />
    The Lys purely functional programming language.
    <br />
    <a href="https://github.com/lyslang/vscode-lys">VS Code Extension</a> •
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
  - Parametric polymorphism
- Practical syntax
  - Concise
  - Elegant
  - Seamless currying support
- Type infered
  - Robust
  - Hindley-Milner type system
- VS Code language support
  - Syntax highlighting
  - Formatting
- Unicode support

## Examples

#### Summation

```lys
def sum lst =
  match lst with \ [] -> 0 \ [hd, tl…] -> hd + sum tl
```

#### Fibonacci

```lys
def fib n : Int -> Int =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)
```

#### Grammar

For a detailed presentation of the Lys syntax and features, please have a look at [our sample grammar file](https://github.com/lyslang/lys/blob/main/examples/grammar.lys).

## Usage

You can skip installation when [running from source](https://github.com/lyslang/lys/blob/main/docs/wiki/from-source.md).

The expected file extension of the Lys programming language is `.lys`.

Following [docopt](http://docopt.org/) conventions:

| Command                | Description                                       |
| :--------------------- | :------------------------------------------------ |
| `lys`                  | Parse standard input and display the AST.         |
| `lys <file>`           | Parse a file and display the AST.                 |
| `lys fmt`              | Format standard input and output it.              |
| `lys fmt <file>`       | Format a file and overwrite it.                   |
| `lys transpile`        | Transpile standard input and output it.           |
| `lys transpile <file>` | Transpile a file and write its OCaml counterpart. |

## Installation

If you are using [NixOS](https://nixos.org/), please follow [these steps](https://github.com/lyslang/lys/blob/main/docs/wiki/nixos.md).

Or, download [the latest executable](https://github.com/lyslang/lys/releases/latest) for your system and [add it to your PATH](https://github.com/lyslang/lys/blob/main/docs/wiki/path.md).

On Unix-like operating systems, you will then need to run the following command:

```sh
chmod +x /path/to/lys/executable
```

## Contributing

We welcome contributions to Lys! If you’re interested in helping improve the language, fix bugs, or add new features, please check out our [contributing guide](https://github.com/lyslang/lys/blob/main/docs/CONTRIBUTING.md) for detailed information on how to get started, set up your development environment, and submit your contributions.

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
