module ExternalFuncs
  ( ExternFuncs(MkExternFuncs)
  , mpInitF
  , mallocF
  , printfF
  , mpReadRadixF
  , mpToRadixF
  , mpRadixSizeF
  , Func(MkFunc)
  , fname
  , ftype
  , registerExtFuncs
  ) where

import Data.Text (Text, unpack)
import LLVM.AST.Name (mkName)
import LLVM.AST.Type (Type, Type(FunctionType), i32, i64, i8, ptr)
import LLVM.IRBuilder.Module (MonadModuleBuilder, extern, externVarArgs)

import ExternalTypes (ExtTypes, mpStruct)

data Func = MkFunc
  { fname :: Text
  , ftype :: Type
  }
  deriving Show

data ExternFuncs = MkExternFuncs
  { mpInitF      :: Func
  , mallocF      :: Func
  , printfF      :: Func
  , mpReadRadixF :: Func
  , mpToRadixF   :: Func
  , mpRadixSizeF :: Func
  }
  deriving Show

registerExtFuncs :: (MonadModuleBuilder m) => ExtTypes -> m ExternFuncs
registerExtFuncs types = do
  let mpStructP = ptr $ mpStruct types
  let i8p       = ptr i8
  let i32p      = ptr i32
  mpInitF      <- regExtern "mp_init" [mpStructP] i32
  mallocF      <- regExtern "malloc" [i64] i8p
  printfF      <- regExternVarArgs "printf" [i8p] i32
  mpReadRadixF <- regExtern "mp_read_radix" [mpStructP, i8p, i32] i32
  mpToRadixF   <- regExtern "mp_toradix" [mpStructP, i8p, i32] i32
  mpRadixSizeF <- regExtern "mp_radix_size" [mpStructP, i32, i32p] i32
  return $ MkExternFuncs { mpInitF, mallocF, printfF, mpReadRadixF, mpToRadixF, mpRadixSizeF }

regExtern :: (MonadModuleBuilder m) => Text -> [Type] -> Type -> m Func
regExtern fname args result = do
  let tname = mkTName fname
  let ftype = ptr $ FunctionType result args False
  extern tname args result
  return MkFunc { fname, ftype }

regExternVarArgs :: (MonadModuleBuilder m) => Text -> [Type] -> Type -> m Func
regExternVarArgs fname args result = do
  let tname = mkTName fname
  let ftype = ptr $ FunctionType result args True
  externVarArgs tname args result
  return MkFunc { fname, ftype }

mkTName = mkName . unpack
