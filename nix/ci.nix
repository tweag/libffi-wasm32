{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghc ? "ghc8107"
}:
pkgs.callPackage
  ({ callPackage, stdenvNoCC }:
    let
      libffi = callPackage ./libffi.nix { };
      wasi-sdk = import ./wasi-sdk.nix { inherit sources; };
    in
    stdenvNoCC.mkDerivation {
      name = "libffi-wasm32-ci";
      dontUnpack = true;
      nativeBuildInputs = [
        (callPackage ./project.nix {
          ghc = "ghc8107";
        }).libffi-wasm32.components.tests.libffi-wasm32-test
        (callPackage "${sources.hs-nix-tools}/pkgs/wasmtime" { })
      ];
      CC =
        "${wasi-sdk}/bin/clang -std=c11 -Wall -Wextra -Oz -flto -I${libffi}/include -L${libffi}/lib -lffi";
      buildPhase = ''
        libffi-wasm32-test
      '';
      installPhase = "export > $out";
    })
{ }
