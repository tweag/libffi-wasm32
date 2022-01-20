# `libffi-wasm32`

Limited port of `libffi` to pure `wasm32`.

## Limitations compared to vanilla `libffi`

- Function argument count is capped to `4`
- For each function type, closure count is capped to `16`. Both
  constants can be tuned, and exceptions can be made for specific
  function types
- Closure API is modified, closure alloc/prep is done in a single call
- No support for structures or variadic functions yet

## How to use

This is a Haskell cabal project, simply run the `libffi-wasm32`
executable. It'll generate additional C sources in `cbits`. Then
include stuff in `cbits` for your own usage.

## How?

There'll probably be a blog post or something.
