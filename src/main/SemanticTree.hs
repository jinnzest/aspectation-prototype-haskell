{-# LANGUAGE TemplateHaskell #-}

module SemanticTree
  ( FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Function(MkFunction, fBody, fSignature)
  , NameArgs(MkNameArgs, naArgs, naName)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsName, fcsArgs, tmpVarName)
  , Return(NoReturn, Return)
  , Statement(MkNoAssignment, expression)
  , SemanticTree(MkSemanticTree)
  , _semanticTree
  , extractFunctions
  , SemanticModel(MkSemanticModel)
  , tree
  , FunctionHash(MkFunctionHash, functionHash, statementHashes)
  , ExpressionHash(MkExpressionHash, expressionHash, semanticPlace)
  , SemanticPlace(MkSemanticPlace, statementPlace, expressionPlace)
  , StatementHashes
  , Arg(MkValue)
  , semanticTree
  , showArg
  , unpackArg
  , unpackReturn
  , hasReturn
  , tmpVarToReturn
  ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON, ToJSONKey, Value(String), toJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Map as M (Map, toList, elems)
import Data.Text (Text, unpack, pack)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import GHC.Classes (Eq((==)), Ord)
import GHC.Generics (Generic)
import Shared (separator, toJSONValue)

import Location (Ranged(MkRanged), rItem, range)
import Data.Bool ((&&))
import CorePrelude (Num((+)))
import Data.List (length)
import Debug.Trace (trace)
import ShowJ (showJ)

type StatementHashes = [[ExpressionHash]]

data FunctionHash = MkFunctionHash
  { functionHash    :: Text
  , statementHashes :: StatementHashes
  }
  deriving Generic
  deriving ToJSON via Vanilla FunctionHash

instance Eq FunctionHash where
  (==) l r = functionHash l == functionHash r

instance Ord FunctionHash where
  compare l r = compare (functionHash l) (functionHash r)

data SemanticPlace = MkSemanticPlace
  { statementPlace  :: Int
  , expressionPlace :: Int
  }
  deriving (Eq, Generic, Ord, Show)
  deriving ToJSON via Vanilla SemanticPlace

instance ToJSONKey SemanticPlace

data ExpressionHash = MkExpressionHash
  { expressionHash :: Text
  , semanticPlace  :: SemanticPlace
  }
  deriving (Eq, Generic, Ord)
  deriving ToJSON via Vanilla ExpressionHash

newtype SemanticModel =
  MkSemanticModel
    { tree :: SemanticTree
    }
  deriving (Eq, Generic) deriving (ToJSON) via Vanilla SemanticModel

newtype SemanticTree =
  MkSemanticTree
    { _semanticTree :: M.Map FunctionHash [Function]
    }
  deriving (Eq, Generic) deriving (ToJSON) via Vanilla SemanticTree

newtype Arg =
  MkValue Text
  deriving (Generic, Ord) deriving (ToJSON) via Vanilla Arg

instance Eq Arg where
  (==) _ _ = True

extractFunctions :: SemanticTree -> [Function]
extractFunctions MkSemanticTree { _semanticTree = tree } = concat $ elems tree

showArg :: Arg -> String
showArg = unpack . unpackArg

unpackArg :: Arg -> Text
unpackArg (MkValue txt) = txt

hasReturn :: FunctionSignature -> Bool
hasReturn MkFunctionSignature { fsReturn = (Return n) } = True
hasReturn MkFunctionSignature { fsReturn = NoReturn }   = False

unpackReturn :: Return -> Maybe Text
unpackReturn (Return n) = Just n
unpackReturn NoReturn   = Nothing

data FunctionSignature = MkFunctionSignature
  { fsName   :: Text
  , fsArgs   :: [Arg]
  , fsReturn :: Return
  }
  deriving Generic
  deriving ToJSON via Vanilla FunctionSignature

data NameArgs = MkNameArgs
  { naName :: Text
  , naArgs :: [Arg]
  }
  deriving Generic
  deriving ToJSON via Vanilla NameArgs

instance ToJSONKey NameArgs

instance Ord NameArgs where
  compare left@MkNameArgs { naName = ln, naArgs = largs } right@MkNameArgs { naName = rn, naArgs = rargs }
    | ln < rn                     = LT
    | ln > rn                     = GT
    | length largs < length rargs = LT
    | length largs > length rargs = GT
    | otherwise                   = EQ

instance Eq NameArgs where
  (==) left@MkNameArgs { naName = ln, naArgs = largs } right@MkNameArgs { naName = rn, naArgs = rargs } =
    ln == rn && length largs == length rargs

data Function = MkFunction
  { fSignature :: FunctionSignature
  , fBody      :: [Ranged Statement]
  }
  deriving (Eq, Generic)
  deriving ToJSON via Vanilla Function

newtype Statement =
  MkNoAssignment
    { expression :: Expression
    }
  deriving (Eq, Generic) deriving (ToJSON) via Vanilla Statement

data Expression
  = MkFunctionCall
      { fcsName   :: Ranged Text
      , fcsArgs   :: [Expression]
      , tmpVarName :: Maybe Text
      }
  | MkConstantInteger (Maybe Text) (Ranged Integer)
  | MkFunctionArgument (Maybe Text) (Ranged Int)
  deriving (Eq, Ord, Generic)

data Return
  = NoReturn
  | Return Text
  deriving (Generic)

instance ToJSON Return -- where
  -- toJSON NoReturn          = String "NoReturn"
  -- toJSON (Return t) = [aesonQQ|{Return:#{t}}|]

instance ToJSONKey FunctionSignature

instance ToJSONKey FunctionHash

instance Show Statement where
  show (MkNoAssignment expression) = show expression

instance Show Function where
  show = showJ

instance Show Arg where
  show = showJ

instance Show Return where
  show = showJ

instance Show NameArgs where
  show = showJ

instance Show FunctionHash where
  show = showJ

instance Show Expression where
  show = showJ

instance Show SemanticModel where
  show = showJ

instance Show SemanticTree where
  show = showJ

instance Show FunctionSignature where
  show = showJ
instance ToJSON Expression where
  toJSON (MkConstantInteger  n i) = [aesonQQ|{tmpName: #{n}, const: #{i}}|]
  toJSON (MkFunctionArgument n a) = [aesonQQ|{tmpName: #{n}, arg: #{a}}|]
  toJSON MkFunctionCall { fcsName = name, fcsArgs = args, tmpVarName = tvn } =
    [aesonQQ|{fcsName: #{name}, fcsArgs: #{args}, tmpVarName: #{tvn}}|]

instance Ord FunctionSignature where
  compare left@MkFunctionSignature { fsName = ln, fsArgs = largs, fsReturn = lret } right@MkFunctionSignature { fsName = rn, fsArgs = rargs, fsReturn = rret }
    | ln < rn
    = LT
    | ln > rn
    = GT
    | length largs < length rargs
    = LT
    | length largs > length rargs
    = GT
    | lret < rret
    = LT
    | lret > rret
    = GT
    | otherwise
    = EQ

instance Ord Return where
  compare NoReturn   NoReturn   = EQ
  compare (Return _) (Return _) = EQ
  compare NoReturn   (Return _) = LT
  compare (Return _) NoReturn   = GT

instance Eq FunctionSignature where
  (==) left@MkFunctionSignature { fsName = fsNameLeft, fsArgs = argsLeft, fsReturn = leftReturn } right@MkFunctionSignature { fsName = fsNameRight, fsArgs = argsRight, fsReturn = rightReturn }
    = fsNameLeft == fsNameRight && length argsLeft == length argsRight && leftReturn == rightReturn

instance Eq Return where
  (==) NoReturn   NoReturn   = True
  (==) (Return l) (Return r) = True
  (==) _          _          = False

tmpVarToReturn :: Maybe Text -> Return
tmpVarToReturn Nothing     = NoReturn
tmpVarToReturn (Just name) = Return name

$(makeLenses ''SemanticTree)
