{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghc ? "ghc8107"
}:
pkgs.callPackage
  ({ callPackage }:
    (import ../shell.nix { inherit sources pkgs ghc; }).overrideAttrs (_: {
      phases = [ "unpackPhase" "buildPhase" ];
      src = callPackage ./src.nix { };
      buildPhase = ''
        export HOME=$(mktemp -d)
        cabal v2-run libffi-wasm32

        $WASI_SDK_PREFIX/bin/clang \
          -std=c11 \
          -Wall \
          -Wextra \
          -Icbits \
          -c \
          cbits/ffi_call.c \
          -fsyntax-only

        $WASI_SDK_PREFIX/bin/clang \
          -std=c11 \
          -Wall \
          -Wextra \
          -Icbits \
          -c \
          cbits/ffi_closure.c \
          -fsyntax-only

        touch $out
      '';
      allowedReferences = [ ];
    }))
{ }
