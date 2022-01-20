{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghc ? "ghc8107"
}:
pkgs.callPackage
  ({ callPackage, clang-tools, lib, stdenv, util-linux }:
    (callPackage ./nix/project.nix { inherit ghc; }).shellFor {
      packages = ps: with ps; [ libffi-wasm32 ];
      withHoogle = true;
      nativeBuildInputs = lib.attrValues
        (import "${sources.hs-nix-tools}/nix/tools.nix" { inherit ghc; }) ++ [
        clang-tools
        (callPackage "${sources.hs-nix-tools}/pkgs/wasmtime" { })
      ] ++ lib.optionals stdenv.isLinux [ util-linux ];
      exactDeps = true;
      shellHook = lib.optionalString stdenv.isLinux "taskset -pc 0-1000 $$";
      WASI_SDK_PREFIX = import ./nix/wasi-sdk.nix { inherit sources; };
    })
{ }
