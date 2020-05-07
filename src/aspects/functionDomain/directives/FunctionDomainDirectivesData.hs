module FunctionDomainDirectivesData
  ( FunctionDomainDirectives
  , FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainConstraintFrom(MkNegInfinite, MkIntegerFrom)
  , FunctionDomainConstraintTo(MkPosInfinite, MkIntegerTo)
  , input
  , output
  , from
  , to
  ) where

import Prelude (Integer)
import Data.Map (Map, toList)
import Data.Text (Text, unpack, pack)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()
import Data.Map as M (Map, empty, fromList)
import Text.Megaparsec (SourcePos(SourcePos), unPos)
import Text.Show (Show(show))
import Data.Ord (Ord(compare), Ordering(EQ))
import Data.Eq (Eq, Eq((==)))
import Data.Int (Int)
import Data.Maybe (Maybe)

import Shared (toJSONValue)
import AspectsData (Analytics, Directives, FunctionCall)
import ShowJ (showJ)

data FunctionDomainInOutConstraints = MkRangedFunctionDomainInOutConstraint
  { from :: FunctionDomainConstraintFrom
  , to   :: FunctionDomainConstraintTo
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla FunctionDomainInOutConstraints

instance Show FunctionDomainInOutConstraints where
  show = showJ
data FunctionDomainConstraintFrom
  = MkNegInfinite
  | MkIntegerFrom Integer
  deriving (Eq, Ord)

data FunctionDomainConstraintTo
  = MkIntegerTo Integer
  | MkPosInfinite
  deriving (Eq, Ord)

instance ToJSON FunctionDomainConstraintFrom where
  toJSON MkNegInfinite     = String "MkNegInfinite"
  toJSON (MkIntegerFrom i) = toJSONValue i

instance ToJSON FunctionDomainConstraintTo where
  toJSON MkPosInfinite   = String "MkPosInfinite"
  toJSON (MkIntegerTo i) = toJSONValue i

instance Show FunctionDomainConstraintFrom where
  show = showJ


instance Show FunctionDomainConstraintTo where
  show = showJ

data FunctionDomainConstraint = MkFunctionDomainConstraint
  { input  :: M.Map Int [FunctionDomainInOutConstraints]
  , output :: Maybe [FunctionDomainInOutConstraints]
  }
  deriving (Eq, Generic)
  deriving ToJSON via Vanilla FunctionDomainConstraint

data ConstraintProbability a = MkMaybeDefined a
  | MkDefined a deriving (Eq, Generic) deriving (ToJSON) via Vanilla (ConstraintProbability a)

instance Show FunctionDomainConstraint where
  show = showJ

instance (Ord FunctionDomainConstraint) where
  compare a b = let nc = compare (input a) (input b) in if nc == EQ then compare (output a) (output b) else nc

instance Show FunctionCall where
  show = showJ

type FunctionDomainDirectives = Directives FunctionDomainConstraint

instance  {-# OVERLAPPING #-} Show FunctionDomainDirectives where
  show = showJ
