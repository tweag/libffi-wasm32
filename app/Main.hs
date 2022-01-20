import Data.ByteString.Builder
import LibFFI
import UnliftIO
import UnliftIO.Process

main :: IO ()
main = do
  let fts = funcTypeEnum 4
      gen_ffi_call = do
        withBinaryFile "cbits/ffi_call.c" WriteMode $
          \h -> hPutBuilder h $ ffiCallC fts
        callProcess "clang-format" ["-i", "cbits/ffi_call.c"]
      gen_ffi_closure = do
        withBinaryFile "cbits/ffi_closure.c" WriteMode $
          \h -> hPutBuilder h $ ffiClosureC [(i, 0x10, ft) | (i, ft) <- fts]
        callProcess "clang-format" ["-i", "cbits/ffi_closure.c"]
  concurrently_ gen_ffi_call gen_ffi_closure
