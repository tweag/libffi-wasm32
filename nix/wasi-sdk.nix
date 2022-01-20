{ sources ? import ./nix/sources.nix { } }: import "${sources.wasi-sdk}/nix" { }
