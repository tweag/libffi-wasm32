{-# LANGUAGE RecordWildCards #-}

import Data.Bits
import Data.Foldable
import Data.List
import qualified Language.C as C
import qualified Language.C.Analysis as C
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
valTypeToCType vt =
  C.DirectType
    ( case vt of
        I32 -> C.TyIntegral C.TyInt
        I64 -> C.TyIntegral C.TyLLong
        F32 -> C.TyFloating C.TyFloat
        F64 -> C.TyFloating C.TyDouble
    )
    C.noTypeQuals
    C.noAttributes

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

funcTypeEnum :: Int -> [FuncType]
funcTypeEnum k =
  [ FuncType {argTypes = arg_tys, retType = ret_ty}
    | arg_tys <- listEnum k [I32, I64, F32, F64],
      ret_ty <- [Nothing, Just I32, Just I64, Just F32, Just F64]
  ]

ffiCallCase :: FuncType -> String
ffiCallCase ft@FuncType {..} =
  "case "
    <> show (funcTypeEncode ft)
    <> ": "
    <> ( case retType of
           Just rt -> "*((" <> valTypeToCTypeStr rt <> "*)rvalue) = "
           _ -> ""
       )
    <> "(("
    <> funcTypeToCFunPtrTypeStr ft
    <> ")fn)("
    <> intercalate
      ", "
      [ "*((" <> valTypeToCTypeStr vt <> "*)(avalue[" <> show i <> "]))"
        | (i, vt) <- zip [0 ..] argTypes
      ]
    <> "); return;\n"

ffiCallFunc :: [FuncType] -> String
ffiCallFunc fts =
  "void ffi_call(ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue)\n{\nswitch (cif->encoding) {\n"
    <> mconcat [ffiCallCase ft | ft <- fts]
    <> "}\n}\n"

ffiCallC :: [FuncType] -> String
ffiCallC fts = "#include <ffi.h>\n\n" <> ffiCallFunc fts

main :: IO ()
main = putStrLn "Hello, Haskell!"
