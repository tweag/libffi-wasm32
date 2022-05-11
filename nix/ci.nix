{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghc ? "ghc922"
}:
pkgs.callPackage
  ({ callPackage, haskell-nix, stdenvNoCC }:
    let
      libffi = callPackage ./libffi.nix { };
      wasi-sdk = callPackage ./wasi-sdk.nix { };
    in
    stdenvNoCC.mkDerivation {
      name = "libffi-wasm32-ci";
      src = haskell-nix.haskellLib.cleanGit {
        name = "libffi-wasm32-src";
        src = ../.;
        subDir = "cbits_test";
      };
      postPatch = "patchShebangs .";
      nativeBuildInputs = [
        (callPackage ./project.nix {
          ghc = "ghc922";
        }).libffi-wasm32.components.tests.libffi-wasm32-test
        (callPackage "${sources.hs-nix-tools}/pkgs/wasmtime" { })
      ];
      CC =
        "${wasi-sdk}/bin/clang -std=c11 -Wall -Wextra -Oz -flto -I${libffi}/include -L${libffi}/lib -lffi";
      buildPhase = ''
        export HOME=$(mktemp -d)
        ./test.sh
      '';
      installPhase = "export > $out";
    })
{ }
