{ autoPatchelfHook, fetchzip, lib, stdenv }:
stdenv.mkDerivation rec {
  pname = "wasmtime";
  version = "0.35.1";
  src = fetchzip {
    x86_64-linux = {
      url =
        "https://github.com/bytecodealliance/wasmtime/releases/download/v${version}/wasmtime-v${version}-x86_64-linux.tar.xz";
      hash =
        "sha512-4rDy+XQy4PhYrJ9g1YsGwenD/W7eNwERSRqW5UTfR0JmT+AlMTbsQgVxgDux4cQ/LfawKUkWvRpg0H2O5Wu8fA==";
    };
    aarch64-linux = {
      url =
        "https://github.com/bytecodealliance/wasmtime/releases/download/v${version}/wasmtime-v${version}-aarch64-linux.tar.xz";
      hash =
        "sha512-+JqisaBj9rS+dGPhusw3FuvM1jk8ackzVsDe/yQvkh02REK1LmmEslHaPMPwBGR/FANIjpiKKPp2h55BnMK2/Q==";
    };
    x86_64-darwin = {
      url =
        "https://github.com/bytecodealliance/wasmtime/releases/download/v${version}/wasmtime-v${version}-x86_64-macos.tar.xz";
      hash =
        "sha512-QYcThsSVl0DRxOVMdnoHjwlWZJylqrsDDBCwuX4MLoqw6pqWzKSEAKFFn8SuOeoQRt4JFIRz2HKVJqMpZsVUeg==";
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
