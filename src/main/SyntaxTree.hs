module SyntaxTree
  ( Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs)
  , Statement(MkNoAssignment)
  , Expression(MkIdentifier, MkInteger)
  , SyntaxTree
  ) where

import Data.Map (Map)
import Data.Ord (Ord)
import Data.List (intercalate)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Types (Int)
import Prelude (Eq, Integer, Show, ($), (++), (.), foldl, map, show)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON)
import Location (Ranged(MkRanged, range, rItem))
import ShowJ (showJ)

newtype Statement =
  MkNoAssignment [Ranged Expression]
  deriving (Eq, Generic) deriving (ToJSON) via Vanilla Statement

data Expression
  = MkIdentifier Text
  | MkInteger Integer
  deriving (Eq, Generic)deriving (ToJSON) via Vanilla Expression

data FunctionSignature = MkFunctionSignature
  { fsName :: Ranged Text
  , fsArgs :: [Ranged Text]
  }
  deriving (Eq, Generic)
  deriving ToJSON via Vanilla FunctionSignature

data Function = MkFunction
  { fSignature :: Ranged FunctionSignature
  , fBody      :: [Ranged Statement]
  }
  deriving (Eq, Generic)
  deriving ToJSON via Vanilla Function

type SyntaxTree = [Ranged Function]

instance Show Function where
  show = showJ

instance Show FunctionSignature where
  show = showJ

instance Show Expression where
  show (MkIdentifier a) = show a
  show (MkInteger    a) = show a

instance Show Statement where
  show (MkNoAssignment e) = show e
