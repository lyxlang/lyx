# SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
#
# SPDX-License-Identifier: GPL-3.0-only

{
  description = "The Lyx purely functional programming language";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    rec {
      packages = forAllSystems (pkgs: rec {
        default = lyx;
        lyx = pkgs.ocamlPackages.buildDunePackage rec {
          pname = "lyx";
          version = "0.0.0";
          src = self;
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
            description = "The Lyx purely functional programming language";
            homepage = "https://github.com/lyxlang/lyx";
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
            ];
            inherit (pkgs.ocaml.meta) platforms;
          };
        };
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell.override { stdenv = pkgs.fastStdenv; } {
          inputsFrom = pkgs.lib.attrValues packages.${pkgs.system};
          packages =
            with pkgs;
            [
              reuse
              just
              ocamlformat_0_27_0
            ]
            ++ (with ocamlPackages; [
              ocaml-lsp
              utop
            ]);
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt-rfc-style);
    };
}
