module Location
  ( Ranged(MkRanged)
  , range
  , rItem
  , MultiRanged(MkMultiRanged)
  , ranges
  , mrItem
  , Range(MkRange)
  , from
  , to
  , Positioned(MkPositioned)
  , pItem
  , pos
  ) where

import Data.Aeson (ToJSON, Value, encode, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.List (intercalate)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import GHC.Generics (Generic)
import Text.Megaparsec (SourcePos(SourcePos), unPos, sourceColumn, sourceLine, sourceName)
import Text.Megaparsec.Pos (Pos)
import ShowJ (showJ)

data Ranged a = MkRanged
  { range :: Range
  , rItem :: a
  }
  deriving (Eq, Ord, Show, Generic)
  deriving ToJSON via Vanilla (Ranged a)

data MultiRanged a = MkMultiRanged
  { ranges :: [Range]
  , mrItem :: a
  }
  deriving Eq

data Range = MkRange
  { from :: SourcePos
  , to   :: SourcePos
  }
  deriving (Eq, Ord, Show, Generic)
  deriving ToJSON via Vanilla Range

data Positioned a = MkPositioned
  { pItem :: a
  , pos   :: SourcePos
  }
  deriving (Eq, Show, Generic)
  deriving ToJSON via Vanilla (Positioned a)

instance ToJSON SourcePos where
  toJSON (SourcePos _ fl fc) = [aesonQQ|{line: #{unPos fl}, column: #{unPos fc}}|]

instance ToJSON a => Show (MultiRanged a) where
  show = showJ

instance ToJSON a => ToJSON (MultiRanged a) where
  toJSON MkMultiRanged { ranges = ranges, mrItem = mrItem } = [aesonQQ|{ranges:#{ranges}, mrItem:#{mrItem}}|]
