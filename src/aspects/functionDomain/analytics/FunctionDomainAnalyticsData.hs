{-# LANGUAGE TemplateHaskell #-}

module FunctionDomainAnalyticsData
  ( PositionedFunctionCall(MkPositionedFunctionCall, functionCalls, callPos)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, callProbability, functionCall)
  , FunctionDomainAnalytics(MkFunctionDomainAnalytics, _undefinedProbabilities, _constraints)
  , undefinedProbabilities
  , constraints
  , FunctionDomainInOutConstraints(MkFunctionDomainInOutConstraints)
  , UndefinedProbability(MaybeDefined, Defined)
  , CallProbability(MaybeWillBeCalled, WillBeCalled)
  , UndefinedProbabilityAnalytics
  , UndefinedFunctionProbability(MkUndefinedFunctionProbability, resultUndefinedProbability, callUndefinedProbabilities)
  ) where

import Prelude (Integer)
import Data.Map (Map, toList)
import Text.Show (Show(show))
import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Maybe (Maybe)
import Data.Text (Text, unpack)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()
import Control.Lens.TH (makeLenses)
import AspectsData (Analytics, Directives, FunctionCall)
import FunctionDomainDirectivesData as Directives (FunctionDomainConstraint)
import Data.Int (Int)
import SemanticTree (FunctionSignature)
import Data.Set as S (Set)
import Data.List ((++))
import ShowJ (showJ)

data FunctionDomainInOutConstraints = MkFunctionDomainInOutConstraints
  { probability    :: UndefinedProbability
  , allConstraints :: FunctionDomainInOutConstraints
  }
  deriving Eq

type UndefinedProbabilityAnalytics = Analytics UndefinedFunctionProbability
data UndefinedFunctionProbability = MkUndefinedFunctionProbability
  { callUndefinedProbabilities :: Set PositionedFunctionCall
  , resultUndefinedProbability :: Maybe UndefinedProbability
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla UndefinedFunctionProbability

instance Show FunctionDomainAnalytics where
  show = showJ

instance Show UndefinedFunctionProbability where
  show = showJ

data FunctionDomainAnalytics = MkFunctionDomainAnalytics
  { _constraints            :: Map FunctionSignature FunctionDomainConstraint
  , _undefinedProbabilities :: UndefinedProbabilityAnalytics
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla FunctionDomainAnalytics

data PositionedFunctionCall = MkPositionedFunctionCall
  { callPos       :: Int
  , functionCalls :: S.Set FunctionCallWithProbability
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla PositionedFunctionCall

data FunctionCallWithProbability = MkFunctionCallWithProbability
  { callProbability :: CallProbability
  , functionCall    :: FunctionCall
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla FunctionCallWithProbability

data UndefinedProbability = MaybeDefined | Defined deriving (Eq,Ord, Generic) deriving (ToJSON) via Vanilla UndefinedProbability

data CallProbability = MaybeWillBeCalled | WillBeCalled deriving (Eq,Ord, Generic) deriving (ToJSON) via Vanilla CallProbability

instance Show UndefinedProbability where
  show = showJ

instance Show CallProbability where
  show = showJ

type FunctionDomainDirectives = Directives FunctionDomainConstraint

instance Show PositionedFunctionCall where
  show MkPositionedFunctionCall { callPos = cp, functionCalls = fc } = show cp ++ ", " ++ show fc

instance Show FunctionCallWithProbability where
  show = showJ

$(makeLenses ''FunctionDomainAnalytics)
