# SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
# SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
#
# SPDX-License-Identifier: GPL-3.0-only

{
  description = "The Lys purely functional programming language";

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
          lys = pkgs.ocamlPackages.buildDunePackage rec {
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
            buildPhase = ''
              runHook preBuild
              dune build --profile release -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
              runHook postBuild
            '';
            checkPhase = ''
              runHook preCheck
              dune runtest --profile release -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
              runHook postCheck
            '';
            installPhase = ''
              runHook preInstall
              dune install --profile release --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname} --docdir $out/share/doc --mandir $out/share/man
              runHook postInstall
            '';
            meta = {
              description = "The Lys purely functional programming language";
              homepage = "https://github.com/lyslang/lys";
              license = with pkgs.lib.licenses; [
                gpl3Only
                cc-by-sa-40
                cc0
              ];
              maintainers = [
                {
                  name = "Aljebriq";
                  email = "143266740+aljebriq@users.noreply.github.com";
                  github = "aljebriq";
                  githubId = 143266740;
                  keys = [
                    {
                      fingerprint = "D6A9 9003 3864 3E39 32FE  2720 2DA5 4C53 EEAB 3434";
                    }
                  ];
                }
                {
                  name = "Łukasz Bartkiewicz";
                  email = "lukasku@proton.me";
                  github = "lokasku";
                  githubId = 105018247;
                }
              ];
              inherit (pkgs.ocaml.meta) platforms;
            };
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
