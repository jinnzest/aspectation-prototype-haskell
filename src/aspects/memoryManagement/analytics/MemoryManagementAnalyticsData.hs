module MemoryManagementAnalyticsData
  ( MemoryManagementAnalytics
  , MemoryManagement(MkMemoryManagement, toCallCases, lineMemoryManagement)
  , ResourceManagement(MkResourceManagement, action, resource)
  , StatementMemoryManagement(MkStatementMemoryManagement, callPos, resources)
  , ToCallCases(MkToCallCases, toCallArgs, toCallRet, hiddenArgs)
  , Resource(MkArg, MkRet, MkVar, MkHiddenArg)
  , Action(Alloc, Free)
  ) where

import Data.Map (Map)
import AspectsData (Analytics, FunctionCall, Analytics)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()
import GHC.Classes (Eq((==)), Ord(compare))
import Data.Ord (Ord)
import Data.Set (Set)
import Data.Text (Text)
import MemoryManagementDirectivesData (MemoryManagementDirective)
import ShowJ (showJ)
import SemanticTree (Expression)

type MemoryManagementAnalytics = Analytics MemoryManagement

data Action = Alloc | Free  deriving (Eq, Ord, Generic) deriving ToJSON via Vanilla Action

data MemoryManagement = MkMemoryManagement
  { toCallCases          :: ToCallCases
  , lineMemoryManagement :: [StatementMemoryManagement]
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla MemoryManagement

data ToCallCases = MkToCallCases
  { toCallArgs :: Map Int (Set MemoryManagementDirective)
  , toCallRet  :: Set MemoryManagementDirective
  , hiddenArgs :: [Text]
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla ToCallCases

data StatementMemoryManagement = MkStatementMemoryManagement
  { callPos   :: Int
  , resources :: [ResourceManagement]
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla StatementMemoryManagement

data ResourceManagement = MkResourceManagement
  { action   :: Set Action
  , resource :: Resource
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla ResourceManagement

data Resource = MkArg{ arg:: Int} | MkRet Expression | MkVar{varName::Text} | MkHiddenArg {hiddenArg:: Text}
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla Resource

instance Show MemoryManagement where
  show = showJ
