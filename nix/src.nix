{ haskell-nix }:
haskell-nix.haskellLib.cleanGit {
  name = "libffi-wasm32-src";
  src = ../.;
}
