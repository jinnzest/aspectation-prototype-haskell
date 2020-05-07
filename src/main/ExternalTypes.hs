module ExternalTypes
  ( ExtTypes(ExtTypes)
  , mpStruct
  , registerExtTypes
  ) where

import Data.Map as M (Map, fromList)
import LLVM.AST.Name (mkName)
import LLVM.AST.Type (Type)
import LLVM.IRBuilder.Module (MonadModuleBuilder, typedef)
import Prelude (Maybe(Nothing), Show, ($), return)

newtype ExtTypes =
  ExtTypes
    { mpStruct :: Type
    }
  deriving (Show)

registerExtTypes :: (MonadModuleBuilder m) => m ExtTypes
registerExtTypes = do
  mpStruct <- typedef (mkName "mp_struct") Nothing
  return $ ExtTypes { mpStruct }
