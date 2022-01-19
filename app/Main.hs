{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString.Builder
import Data.Foldable
import Data.List
import qualified Language.C as C
import qualified Language.C.Analysis as C
import System.IO
import System.Process
import Text.PrettyPrint
  ( Mode (..),
    Style (..),
    renderStyle,
    style,
  )

data ValType
  = I32
  | I64
  | F32
  | F64
  deriving (Show)

valTypeToCType :: ValType -> C.Type
valTypeToCType I32 =
  C.TypeDefType
    ( C.TypeDefRef
        (C.internalIdent "uint32_t")
        (C.DirectType (C.TyIntegral C.TyUInt) C.noTypeQuals C.noAttributes)
        C.undefNode
    )
    C.noTypeQuals
    C.noAttributes
valTypeToCType I64 =
  C.TypeDefType
    ( C.TypeDefRef
        (C.internalIdent "uint64_t")
        (C.DirectType (C.TyIntegral C.TyULLong) C.noTypeQuals C.noAttributes)
        C.undefNode
    )
    C.noTypeQuals
    C.noAttributes
valTypeToCType F32 =
  C.DirectType (C.TyFloating C.TyFloat) C.noTypeQuals C.noAttributes
valTypeToCType F64 =
  C.DirectType (C.TyFloating C.TyDouble) C.noTypeQuals C.noAttributes

valTypeToCTypeStr :: ValType -> String
valTypeToCTypeStr =
  renderStyle (style {mode = OneLineMode}) . C.pretty . valTypeToCType

data FuncType = FuncType
  { argTypes :: [ValType],
    retType :: Maybe ValType
  }
  deriving (Show)

funcTypeToCFunPtrType :: FuncType -> C.Type
funcTypeToCFunPtrType FuncType {..} =
  C.PtrType
    ( C.FunctionType
        ( C.FunType
            ( maybe
                (C.DirectType C.TyVoid C.noTypeQuals C.noAttributes)
                valTypeToCType
                retType
            )
            [ C.ParamDecl
                ( C.VarDecl
                    C.NoName
                    (C.DeclAttrs C.noFunctionAttrs C.NoStorage C.noAttributes)
                    (valTypeToCType arg_ty)
                )
                C.undefNode
              | arg_ty <- argTypes
            ]
            False
        )
        C.noAttributes
    )
    C.noTypeQuals
    C.noAttributes

funcTypeToCFunPtrTypeStr :: FuncType -> String
funcTypeToCFunPtrTypeStr =
  renderStyle (style {mode = OneLineMode}) . C.pretty . funcTypeToCFunPtrType

funcTypeEncode :: FuncType -> Word
funcTypeEncode FuncType {..} = r * 5 + tag
  where
    (r, _) =
      foldl'
        ( \(acc, rdx) ty ->
            ( acc
                + rdx
                * ( case ty of
                      I32 -> 1
                      I64 -> 2
                      F32 -> 3
                      F64 -> 4
                  ),
              rdx * 5
            )
        )
        (0, 1)
        argTypes
    tag = case retType of
      Nothing -> 0
      Just I32 -> 1
      Just I64 -> 2
      Just F32 -> 3
      Just F64 -> 4

listEnum :: Int -> [a] -> [[a]]
listEnum k l =
  mconcat $ take (k + 1) $ iterate (\acc -> [x : xs | x <- l, xs <- acc]) [[]]

funcTypeEnum :: Int -> [(Word, FuncType)]
funcTypeEnum k =
  sortOn
    fst
    [ (funcTypeEncode ft, ft)
      | arg_tys <- listEnum k [I32, I64, F32, F64],
        ret_ty <- [Nothing, Just I32, Just I64, Just F32, Just F64],
        let ft = FuncType {argTypes = arg_tys, retType = ret_ty}
    ]

ffiCallCase :: Word -> FuncType -> Builder
ffiCallCase i ft@FuncType {..} =
  "case 0x"
    <> wordHex i
    <> ": "
    <> ( case retType of
           Just rt -> "*((" <> string7 (valTypeToCTypeStr rt) <> "*)rvalue) = "
           _ -> ""
       )
    <> "(("
    <> string7 (funcTypeToCFunPtrTypeStr ft)
    <> ")fn)("
    <> mconcat
      ( intersperse
          ", "
          [ "*(("
              <> string7 (valTypeToCTypeStr vt)
              <> "*)(avalue[0x"
              <> wordHex i
              <> "]))"
            | (i, vt) <- zip [0 ..] argTypes
          ]
      )
    <> "); return;\n"

ffiCallFunc :: [(Word, FuncType)] -> Builder
ffiCallFunc fts =
  "void ffi_call(ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue)\n{\nswitch (cif->encoding) {\n"
    <> mconcat [ffiCallCase i ft | (i, ft) <- fts]
    <> "}\n}\n"

ffiCallC :: [(Word, FuncType)] -> Builder
ffiCallC fts = "#include <ffi.h>\n\n" <> ffiCallFunc fts

main :: IO ()
main = do
  withBinaryFile "cbits/ffi_call.c" WriteMode $
    \h -> hPutBuilder h $ ffiCallC $ funcTypeEnum 4
  callProcess "clang-format" ["-i", "cbits/ffi_call.c"]
