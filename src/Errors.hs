module Errors
  ( Errors(MkErrors)
  , errors
  , Error(MkMultiRangedError, MkRangedError, MkPositionedError, MkError)
  ) where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos(..), mkPos)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON)

import Location (MultiRanged, Positioned, Ranged)
import ShowJ (showJ)

newtype Errors =
  MkErrors
    { errors :: [Error]
    }
  deriving (Eq, Generic)  deriving (ToJSON) via Vanilla Errors

instance Show Errors where
  show = showJ

data Error
  = MkMultiRangedError (MultiRanged Text)
  | MkRangedError (Ranged Text)
  | MkPositionedError (Positioned Text)
  | MkError Text
  deriving (Eq, Generic)  deriving (ToJSON) via Vanilla Error

instance Show Error where
  show = showJ
