module EmbeddedFuncs
  ( EmbeddedFuncs(MkEmbeddedFuncs, printF)
  , registerEmbeddedFuncs
  , embeddedSignatures
  ) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import SemanticTree (Arg(MkValue), FunctionSignature(MkFunctionSignature), Return(NoReturn), fsArgs, fsName, fsReturn)

newtype EmbeddedFuncs =
  MkEmbeddedFuncs
    { printF :: FunctionSignature
    }
  deriving (Generic)

registerEmbeddedFuncs :: EmbeddedFuncs
registerEmbeddedFuncs =
  MkEmbeddedFuncs { printF = MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn } }

embeddedSignatures :: [FunctionSignature]
embeddedSignatures = [printF registerEmbeddedFuncs]
