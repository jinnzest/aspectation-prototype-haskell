module ValueSizeData
  ( ValueSizeAnalyticsValues(MkValueSizeAnalyticsValues, argumentSizes, internalSizes, resultSize, callArgSizes)
  , ValueSizeAnalyticsValue(Pointer, Bits)
  , ValueSizeAnalytics
  , ArgumentSizes
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithArgSizes(MkFunctionCallWithSizes, argSizes, retSize, functionCall)
  ) where

import Data.Map (Map)
import AspectsData (Analytics, FunctionCall)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String), ToJSONKey)
import Data.Aeson.QQ ()
import GHC.Classes (Eq((==)), Ord(compare))
import Data.Ord (Ord)
import Data.Set (Set)
import Data.Aeson.Types (ToJSONKey(toJSONKey))
import SemanticTree (SemanticPlace)
import ShowJ (showJ)

type ArgumentSizes = Map Int ValueSizeAnalyticsValue
type InternalSizes = Map SemanticPlace ValueSizeAnalyticsValue

instance ToJSONKey ArgumentSizes

instance ToJSONKey InternalSizes

data ValueSizeAnalyticsValues = MkValueSizeAnalyticsValues
  { argumentSizes :: ArgumentSizes
  , internalSizes :: InternalSizes
  , resultSize    :: Maybe ValueSizeAnalyticsValue
  , callArgSizes  :: Set PositionedFunctionCall
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla ValueSizeAnalyticsValues

data FunctionCallWithArgSizes = MkFunctionCallWithSizes
  { argSizes     :: [ValueSizeAnalyticsValue]
  , retSize      :: Maybe ValueSizeAnalyticsValue
  , functionCall :: FunctionCall
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla FunctionCallWithArgSizes

data PositionedFunctionCall = MkPositionedFunctionCall
  { callPos       :: Integer
  , functionCalls :: [FunctionCallWithArgSizes]
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla PositionedFunctionCall

type ValueSizeAnalytics = Analytics (Map ArgumentSizes ValueSizeAnalyticsValues)

data ValueSizeAnalyticsValue =
   Bits Int | Pointer
   deriving (Eq, Ord, Show, Generic) deriving (ToJSON) via Vanilla ValueSizeAnalyticsValue

instance Show ValueSizeAnalyticsValues where
  show = showJ

instance   {-# OVERLAPPING #-}  Show ValueSizeAnalytics where
  show = showJ

