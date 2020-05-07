module AspectsData
  ( Analytics
  , Directives
  , FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName)
  , EmptyMap
  ) where

import Data.Map as M (Map, toList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, ToJSONKey)
import SemanticTree (FunctionSignature(MkFunctionSignature), fsName, fsArgs, fsReturn)
import Data.Maybe (isJust)

data FunctionCall = MkFunctionCall
  { fcName     :: Text
  , fcArgs     :: [Text]
  , tmpVarName :: Maybe Text
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla FunctionCall

type EmptyMap = Map FunctionSignature ()

type Analytics a = M.Map FunctionSignature a

type Directives a = M.Map FunctionSignature a
