{ callPackage, clang-tools, stdenvNoCC }:
stdenvNoCC.mkDerivation {
  name = "libffi";
  src = callPackage ./src.nix { };
  nativeBuildInputs = [
    clang-tools
    (callPackage ./project.nix {
      ghc = "ghc8107";
    }).libffi-wasm32.components.exes.libffi-wasm32
    (import ./wasi-sdk.nix { })
  ];
  buildPhase = ''
    libffi-wasm32

    clang -std=c11 -Wall -Wextra -Oz -flto -Icbits -c cbits/ffi.c -o cbits/ffi.o
    clang -std=c11 -Wall -Wextra -Oz -flto -Icbits -c cbits/ffi_call.c -o cbits/ffi_call.o
    clang -std=c11 -Wall -Wextra -Oz -flto -Icbits -c cbits/ffi_closure.c -o cbits/ffi_closure.o

    mkdir -p $out/include
    cp cbits/ffi.h $out/include
    mkdir $out/lib
    llvm-ar -r $out/lib/libffi.a cbits/ffi.o cbits/ffi_call.o cbits/ffi_closure.o
  '';
  dontInstall = true;
  dontFixup = true;
  allowedReferences = [ ];
}
