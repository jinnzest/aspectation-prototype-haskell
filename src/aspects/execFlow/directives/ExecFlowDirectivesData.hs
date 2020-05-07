module ExecFlowDirectivesData
  ( ExecFlowDirectives
  , Visibility(Pub, Priv)
  ) where

import AspectsData (Directives)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()

data Visibility = Pub | Priv deriving (Eq, Show, Generic) deriving (ToJSON) via Vanilla Visibility

type ExecFlowDirectives = Directives Visibility
