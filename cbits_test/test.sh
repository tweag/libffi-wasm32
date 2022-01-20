#!/usr/bin/env bash

set -euox pipefail

$CC call.c -o call.wasm

wasmtime call.wasm

$CC closure.c -o closure.wasm

wasmtime closure.wasm
