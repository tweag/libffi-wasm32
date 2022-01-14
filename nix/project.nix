{ ghc, haskell-nix }:
haskell-nix.cabalProject {
  src = haskell-nix.haskellLib.cleanGit {
    name = "libffi-wasm32-src";
    src = ../.;
  };
  compiler-nix-name = ghc;
}
