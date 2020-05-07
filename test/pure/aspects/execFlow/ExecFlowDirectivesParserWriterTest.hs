module ExecFlowDirectivesParserWriterTest
  ( execFlowDirectivesParserWriterTest
  , testParseExecFlowDirectives
  ) where

import Prelude ()
import SemanticTreeArbitrary (maxItemsSize)
import Control.Monad.Except (Except, runExceptT, mapExceptT)
import Data.Map as M (Map, empty, fromList, lookup, member, toList)
import Data.List as L (sortBy, maximumBy, null, map, filter, length, zipWith, zip)
import Data.Text (Text, append, pack, unpack)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, choose, oneof, testProperty)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Pos (SourcePos(SourcePos), mkPos, sourceColumn, sourceLine, sourceName)
import Data.Either (Either(Right), Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Control.Monad (Monad(return))
import Data.Function ((.), ($))
import Data.List ((++))
import Text.Show (Show(show))
import Data.Functor ((<$>))
import Data.Bool (not, Bool(True))
import Data.Ord (Ord(compare))
import Data.Tuple (fst)
import GHC.Num (Num((+)))
import Data.Eq (Eq((/=)), Eq((==)))
import Control.Monad.Identity (Identity(runIdentity))
import Test.Tasty.HUnit (testCase, assertEqual)

import AspectsData as Asp (Analytics, Directives)
import Errors (Error(MkError), Errors(MkErrors))
import ExecFlowDirectivesParser (execFlowDirectivesParser)
import Location as Loc (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import Panic (panic)
import ParserWrapper (parse)
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , NameArgs(MkNameArgs, naArgs, naName)
  , Return(NoReturn, Return)
  , Return(NoReturn, Return)
  , FunctionHash(MkFunctionHash)
  , SemanticModel(MkSemanticModel)
  , SemanticTree(MkSemanticTree)
  , Statement(MkNoAssignment)
  , fsArgs
  , fsName
  , fsReturn
  )
import ShowJ (showJ)
import TestShared (parseStep, testParse)
import Data.Int (Int)
import ExecFlowDirectivesData (ExecFlowDirectives, Visibility(Pub, Priv))
import ExecFLowDirectivesWriter (writeExecFlowDirectives)

execFlowDirectivesParserWriterTest :: TestTree
execFlowDirectivesParserWriterTest = testGroup
  "Execution flow directives tests"
  [ testProperty "read the same execution flow directives as were written" $ \analytics ->
      let
        writtenDirectives = fromList analytics
        body              = writeExecFlowDirectives writtenDirectives
        readDirectives    = testParseExecFlowDirectives body
      in writtenDirectives == readDirectives
  ]

extractRight (Right r) = r
extractRight other     = panic ("unexpected: " ++ show other)

-- instance Arbitrary FunctionSignature where
--   arbitrary = do
--     size     <- elements [0 .. 4]
--     fsName   <- arbitrary
--     fsArgs   <- vectorOf size arbitrary
--     fsReturn <- arbitrary
--     return $ MkFunctionSignature { fsName, fsArgs, fsReturn }

instance Arbitrary Visibility where
  arbitrary = oneof [return Pub, return Priv]

testParseExecFlowDirectives :: Text -> ExecFlowDirectives
testParseExecFlowDirectives body =
  let
    parsed = parse "" body execFlowDirectivesParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, Visibility)])
  in M.fromList $ map rItem $ extractRight $ extractRight eitherResult
