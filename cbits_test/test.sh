#!/bin/sh

set -eu

$CC call.c -o call.wasm

wasmtime run --disable-cache call.wasm

$CC closure.c -o closure.wasm

wasmtime run --disable-cache closure.wasm
