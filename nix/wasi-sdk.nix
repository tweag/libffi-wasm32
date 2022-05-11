{ autoPatchelfHook, fetchzip, gcc9Stdenv, libxml2, ncurses }:
gcc9Stdenv.mkDerivation {
  name = "wasi-sdk";

  src = fetchzip {
    url =
      "https://nightly.link/WebAssembly/wasi-sdk/actions/artifacts/236301204.zip";
    hash =
      "sha512-36jUV0o2sK4DPCOBF+lmFImycu+no84zorzb59YdUxpt3nAH8aK8FHLQs1epGoop7px2aSlKZQ+aYlZFLYpWGA==";
    stripRoot = false;
  };

  buildInputs = [ gcc9Stdenv.cc.cc.lib libxml2 ncurses ];
  nativeBuildInputs = [ autoPatchelfHook ];

  installPhase = ''
    mkdir $out
    tar xzf wasi-sdk-*.tar.gz --strip-components=1 -C $out
    autoPatchelf $out/bin
  '';

  dontFixup = true;

  doInstallCheck = true;
  installCheckPhase = "$out/bin/wasm-ld --version";

  strictDeps = true;
}
