{ callPackage, ghc, haskell-nix }:
haskell-nix.cabalProject {
  src = callPackage ./src.nix { };
  compiler-nix-name = ghc;
}
