# SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
# SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
#
# SPDX-License-Identifier: GPL-3.0-only

{
  description = "The Lys purely functional programming language.";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      rec {
        packages = rec {
          default = lys;
          lys = pkgs.ocamlPackages.buildDunePackage {
            pname = "lys";
            version = "0.0.0";
            src = ./.;
            strictDeps = false;
            stdenv = pkgs.fastStdenv;
            buildInputs = with pkgs.ocamlPackages; [
              sedlex
              menhir
              menhirLib
              ppx_deriving
              uuseg
              yojson
            ];
          };
        };

        devShells.default = pkgs.mkShell.override { stdenv = pkgs.fastStdenv; } {
          inputsFrom = pkgs.lib.attrValues packages;
          packages =
            with pkgs;
            [
              reuse
              just
              haskellPackages.BNFC
              ocamlformat_0_27_0
            ]
            ++ (with pkgs.ocamlPackages; [
              ocaml-lsp
              utop
            ]);
        };
      }
    );
}
