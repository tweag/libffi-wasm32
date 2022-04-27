{ autoPatchelfHook, fetchzip, lib, stdenv }:
stdenv.mkDerivation rec {
  pname = "wasmtime";
  version = "0.36.0";
  src = fetchzip {
    x86_64-linux = {
      url =
        "https://github.com/bytecodealliance/wasmtime/releases/download/v${version}/wasmtime-v${version}-x86_64-linux.tar.xz";
      hash =
        "sha512-YYkaFE1u/nfHtq/Pm1KaLfHqtycCA95Q0y6uGfbRKOf77xzdUmU+I0EPRT4L9sgnFUZpmd3L3KIl7sXMP06eWw==";
    };
    aarch64-linux = {
      url =
        "https://github.com/bytecodealliance/wasmtime/releases/download/v${version}/wasmtime-v${version}-aarch64-linux.tar.xz";
      hash =
        "sha512-Tq8TE/eZ3EZAO0wnW8BuoZ794M/SmSaczmWHKvM7BMzGzkGLoZrB0sSrS9WJfpGKCkglfk5/a24/vXjKnIwTxg==";
    };
    x86_64-darwin = {
      url =
        "https://github.com/bytecodealliance/wasmtime/releases/download/v${version}/wasmtime-v${version}-x86_64-macos.tar.xz";
      hash =
        "sha512-tMAoUZCBJB6p3cLqK4LFZqmutzRWvw+dSjBR62oI4L0cCYEu5UzjxHwPMEqEJ5EFMgPlEUOPqFtyi4lrxNZgCA==";
    };
  }.${stdenv.hostPlatform.system};
  nativeBuildInputs = lib.optionals stdenv.isLinux [ autoPatchelfHook ];
  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 -t $out/bin wasmtime
  '';
  doInstallCheck = true;
  installCheckPhase = "$out/bin/wasmtime --version";
  strictDeps = true;
}
