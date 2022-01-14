{-# LANGUAGE RecordWildCards #-}

import qualified Language.C as C
import qualified Language.C.Analysis as C
import Text.PrettyPrint

data ValType
  = I32
  | I64
  | F32
  | F64

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

data FuncType = FuncType
  { argTypes :: [ValType],
    retType :: Maybe ValType
  }

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
