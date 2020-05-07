module MemoryManagementDirectivesData
  ( StatementMemoryManagementDirectives(MkStatementMemoryManagementDirectives, statementMemoryManagement, statementPos)
  , ExpressionMemoryManagementDirectives
    ( MkCallMemoryManagementDirectives
    , MkVariableMemoryManagementDirective
    , argumentsManagement
    , functionSignature
    , returnManagement
    )
  , MemoryManagementDirective(AllocHere, AllocAbove)
  , MemoryManagementDirectives
  ) where

import Prelude ()
import Data.Map (Map)
import AspectsData (Analytics, FunctionCall, Directives)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson.QQ ()
import GHC.Classes (Eq((==)), Ord(compare))
import Data.Ord (Ord)
import Data.Text (Text, pack, unpack)
import SemanticTree (FunctionSignature)
import Text.Show (Show(show))
import Data.Int (Int)
import CorePrelude (Integer)
import Data.Maybe (Maybe)
import Data.Function (($), (.))
import ShowJ (showJ)

type MemoryManagementDirectives = Directives [StatementMemoryManagementDirectives]

data MemoryManagementDirective = AllocHere | AllocAbove
   deriving (Eq, Ord, Show, Generic) deriving (ToJSON) via Vanilla MemoryManagementDirective

data StatementMemoryManagementDirectives = MkStatementMemoryManagementDirectives
  { statementPos              :: Int
  , statementMemoryManagement :: [ExpressionMemoryManagementDirectives]
  }
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla StatementMemoryManagementDirectives

instance Show StatementMemoryManagementDirectives where
  show = showJ

data ExpressionMemoryManagementDirectives =  MkCallMemoryManagementDirectives
  { functionSignature   :: FunctionSignature
  , argumentsManagement :: Map Int MemoryManagementDirective
  , returnManagement    :: Maybe MemoryManagementDirective
  } | MkVariableMemoryManagementDirective  Text MemoryManagementDirective
  deriving (Eq, Ord, Generic)
  deriving ToJSON via Vanilla ExpressionMemoryManagementDirectives

instance Show ExpressionMemoryManagementDirectives where
  show = showJ
