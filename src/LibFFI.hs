{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LibFFI where

import Data.ByteString.Builder
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

data FuncType = FuncType
  { argTypes :: [ValType],
    retType :: Maybe ValType
  }
  deriving (Show)

funcTypeToCType :: FuncType -> C.Type
funcTypeToCType FuncType {..} =
  C.FunctionType
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

funcTypeToCFunPtrType :: FuncType -> C.Type
funcTypeToCFunPtrType ft =
  C.PtrType (funcTypeToCType ft) C.noTypeQuals C.noAttributes

cToStr :: C.Pretty a => a -> String
cToStr = renderStyle (style {mode = OneLineMode}) . C.pretty

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
           Just rt ->
             "*((" <> string7 (cToStr (valTypeToCType rt)) <> "*)rvalue) = "
           _ -> ""
       )
    <> "(("
    <> string7 (cToStr (funcTypeToCFunPtrType ft))
    <> ")fn)("
    <> mconcat
      ( intersperse
          ", "
          [ "*(("
              <> string7 (cToStr (valTypeToCType vt))
              <> "*)(avalue[0x"
              <> wordHex j
              <> "]))"
            | (j, vt) <- zip [0 ..] argTypes
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

ffiPoolClosureArrName :: Word -> Builder
ffiPoolClosureArrName i = "ffi_pool_closure_" <> wordHex i

ffiPoolClosureArrDef :: Word -> Word -> Builder
ffiPoolClosureArrDef i m =
  "static ffi_closure "
    <> ffiPoolClosureArrName i
    <> "[0x"
    <> wordHex m
    <> "];\n"

ffiPoolClosure :: Word -> Word -> Builder
ffiPoolClosure i j = ffiPoolClosureArrName i <> "[0x" <> wordHex j <> "]"

ffiPoolFuncName :: Word -> Word -> Builder
ffiPoolFuncName i j = "ffi_pool_func_" <> wordHex i <> "_" <> wordHex j

ffiPoolFuncDef :: Word -> Word -> FuncType -> Builder
ffiPoolFuncDef i j FuncType {..} =
  "static "
    <> ( case retType of
           Just rt -> string7 (cToStr (valTypeToCType rt))
           _ -> "void"
       )
    <> " "
    <> ffiPoolFuncName i j
    <> "("
    <> mconcat
      ( intersperse
          ","
          [ string7 (cToStr (valTypeToCType arg_ty)) <> " a" <> wordHex k
            | (k, arg_ty) <- zip [0 ..] argTypes
          ]
      )
    <> ")\n{\n"
    <> ( if null argTypes
           then mempty
           else
             "void *args[] = {"
               <> mconcat
                 ( intersperse
                     ","
                     ["&a" <> wordHex k | (k, _) <- zip [0 ..] argTypes]
                 )
               <> "};\n"
       )
    <> ( case retType of
           Just rt -> string7 (cToStr (valTypeToCType rt)) <> " ret;\n"
           _ -> mempty
       )
    <> ffiPoolClosure i j
    <> ".fun("
    <> ffiPoolClosure i j
    <> ".cif,"
    <> ( case retType of
           Just _ -> "&ret,"
           _ -> "NULL,"
       )
    <> (if null argTypes then "NULL," else "args,")
    <> ffiPoolClosure i j
    <> ".user_data"
    <> ");\n"
    <> ( case retType of
           Just _ -> "return ret;\n"
           _ -> mempty
       )
    <> "}\n"

ffiPoolFuncArrName :: Word -> Builder
ffiPoolFuncArrName i = "ffi_pool_func_" <> wordHex i

ffiPoolFuncArrDef :: Word -> Word -> Builder
ffiPoolFuncArrDef i m =
  "static void *"
    <> ffiPoolFuncArrName i
    <> "[] = {"
    <> mconcat (intersperse "," [ffiPoolFuncName i j | j <- [0 .. m - 1]])
    <> "};\n"

ffiClosureFreeDef :: Builder
ffiClosureFreeDef =
  "void ffi_closure_free(void *c)\n{\n((ffi_closure*)c)->fun=NULL;\n}\n"

ffiPoolAllocDef :: Builder
ffiPoolAllocDef =
  "static ffi_closure *ffi_pool_alloc(ffi_closure cs[], void *fs[],\n"
    <> "unsigned short m,\n"
    <> "void **f) {\n"
    <> "for (unsigned short j = 0; j < m; ++j) {\n"
    <> "if (cs[j].fun == NULL) {\n"
    <> "*f = fs[j];\n"
    <> "return cs + j;\n"
    <> "}\n"
    <> "}\n"
    <> "return NULL;\n"
    <> "}\n"

ffiAllocPrepClosureCase :: Word -> Word -> Builder
ffiAllocPrepClosureCase i m =
  "case 0x"
    <> wordHex i
    <> ":\n"
    <> "*pclosure=ffi_pool_alloc("
    <> ffi_closure_arr
    <> ","
    <> ffi_func_arr
    <> ",0x"
    <> wordHex m
    <> ",code);\nbreak;\n"
  where
    ffi_closure_arr = ffiPoolClosureArrName i
    ffi_func_arr = ffiPoolFuncArrName i

ffiAllocPrepClosureDef :: [(Word, Word, FuncType)] -> Builder
ffiAllocPrepClosureDef fts =
  "ffi_status ffi_alloc_prep_closure(ffi_closure **pclosure, ffi_cif *cif, void (*fun)(ffi_cif *cif, void *ret, void **args, void *user_data), void *user_data, void **code)\n{*pclosure=NULL;\nswitch(cif->encoding){\n"
    <> mconcat [ffiAllocPrepClosureCase i m | (i, m, _) <- fts]
    <> "default: return FFI_BAD_TYPEDEF;\n"
    <> "}"
    <> "if(*pclosure==NULL) { return FFI_CLOSURE_ALLOC_FAIL; }\n"
    <> "(*pclosure)->cif = cif;\n"
    <> "(*pclosure)->fun = fun;\n"
    <> "(*pclosure)->user_data = user_data;\n"
    <> "return FFI_OK;\n"
    <> "}\n"

ffiClosureC :: [(Word, Word, FuncType)] -> Builder
ffiClosureC fts =
  "#include <ffi.h>\n\n"
    <> mconcat
      [ ffiPoolClosureArrDef i m
          <> mconcat [ffiPoolFuncDef i j ft | j <- [0 .. m - 1]]
          <> ffiPoolFuncArrDef i m
        | (i, m, ft) <- fts
      ]
    <> ffiClosureFreeDef
    <> ffiPoolAllocDef
    <> ffiAllocPrepClosureDef fts
