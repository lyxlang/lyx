# SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
#
# SPDX-License-Identifier: GPL-3.0-only

{
  packages =
    with pkgs;
    lib.optionals stdenv.isDarwin (
      [
        llvm
      ]
      ++ (with darwin.apple_sdk.frameworks; [
        CoreServices
        Foundation
        Security
        SystemConfiguration
      ])
      ++ (with darwin; [
        libiconv
        Libsystem
        cctools
      ])
    );

  shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin (
    with pkgs.darwin;
    ''
      export LIBRARY_PATH=${Libsystem}/lib:$LIBRARY_PATH
      export CPATH=${Libsystem}/include:$CPATH
      export SDKROOT=$(xcrun --show-sdk-path)
      export MACOSX_DEPLOYMENT_TARGET=11.3
    ''
  );
}
