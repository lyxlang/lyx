<!--
SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Installation on NixOS

Add the Lyx input and inherit `inputs` in your `flake.nix` configuration for your system and for [Home Manager](https://nix-community.github.io/home-manager/) if you have it:

```nix
{
  inputs = {
    …
    lyx = {
      url = "github:lyxlang/lyx";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, home-manager, ... }@inputs:
    {
      nixosConfigurations.host = nixpkgs.lib.nixosSystem {
        …
        specialArgs = {
          inherit inputs;
        };
        modules = [
          …
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              …
              extraSpecialArgs = {
                inherit inputs;
              };
            };
          }
        ];
      };
    };
}
```

You can then install Lyx as a system package:

```nix
{ …, pkgs, inputs, ... }:

{
  environment.systemPackages = with pkgs; [
    …
    inputs.lyx.packages.${system}.lyx
  ];
}
```

Or as a user package with [Home Manager](https://nix-community.github.io/home-manager/):

```nix
{ …, pkgs, inputs, ... }:

{
  home.packages = with pkgs; [
    …
    inputs.lyx.packages.${system}.lyx
  ];
}
```
