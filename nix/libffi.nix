{ callPackage, clang-tools, haskell-nix, stdenvNoCC }:
let
  wasi-sdk = import ./wasi-sdk.nix { };
  wasmtime = callPackage ./wasmtime.nix { };
in
stdenvNoCC.mkDerivation {
  name = "libffi";
  outputs = [ "out" "cbits" ];
  src = callPackage ./src.nix { };
  nativeBuildInputs = [
    clang-tools
    (callPackage ./project.nix {
      ghc = "ghc921";
    }).libffi-wasm32.components.exes.libffi-wasm32
    wasmtime
  ];
  buildPhase = ''
    libffi-wasm32
    cp -r cbits $cbits

    ${wasi-sdk}/bin/clang -Wall -Wextra -Oz -DNDEBUG -Icbits -c cbits/ffi.c -o cbits/ffi.o
    ${wasi-sdk}/bin/clang -Wall -Wextra -Oz -DNDEBUG -Icbits -c cbits/ffi_call.c -o cbits/ffi_call.o
    ${wasi-sdk}/bin/clang -Wall -Wextra -Oz -DNDEBUG -Icbits -c cbits/ffi_closure.c -o cbits/ffi_closure.o

    mkdir -p $out/include
    cp cbits/*.h $out/include
    mkdir $out/lib
    ${wasi-sdk}/bin/llvm-ar -r $out/lib/libffi.a cbits/*.o
  '';
  dontInstall = true;
  dontFixup = true;
  allowedReferences = [ ];
  doInstallCheck = true;
  installCheckPhase = ''
    pushd cbits_test
    CC="${wasi-sdk}/bin/clang -Wall -Wextra -Oz -DNDEBUG -I$out/include -L$out/lib -lffi" ./test.sh
    popd
  '';
}
