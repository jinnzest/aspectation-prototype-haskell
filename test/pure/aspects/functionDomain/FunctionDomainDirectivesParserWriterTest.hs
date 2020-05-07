module FunctionDomainDirectivesParserWriterTest
  ( functionDomainDirectivesParserWriterTest
  , testParseFunctionDomainDirectives
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
import FunctionDomainAnalyticsIO (readFunctionDomainAnalytics, writeFunctionDomainAnalytics)
import FunctionDomainDirectives (propagateFunctionDomainDirectives)
import FunctionDomainDirectivesData as FDD
  ( FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainConstraintFrom(MkNegInfinite)
  , FunctionDomainConstraintTo(MkPosInfinite)
  , FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainDirectives
  , from
  , input
  , output
  , to
  )
import FunctionDomainDirectivesParser (functionDomainDirectivesParser)
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
  , unpackArg
  )
import ShowJ (showJ)
import TestShared (parseStep, testParse)
import Data.Int (Int)
import FunctionDomainDirectivesWriters (writeFunctionDomainDirectivesMap)

functionDomainDirectivesParserWriterTest :: TestTree
functionDomainDirectivesParserWriterTest = testGroup
  "Execution flow directives tests"
  [ testProperty "read the same execution flow directives as were written" $ \writtenConstraints ->
      let
        writtenDirectives = fromList writtenConstraints
        body              = writeFunctionDomainDirectivesMap writtenDirectives
        readDirectives    = testParseFunctionDomainDirectives body
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

instance {-# OVERLAPPING #-} Arbitrary (FunctionSignature, FunctionDomainConstraint) where
  arbitrary = do
    fsName    <- arbitrary
    inputList <- arbitrary
    let input = M.fromList inputList
    output <- arbitrary
    let nonEmptyConstraints = MkFunctionDomainConstraint { input, output }
    let fsArgs = map (MkValue . (\n -> pack $ "a" ++ show n)) [1 .. (maxArgsLength inputList + 1)]
    let
      fsReturn = case output of
        Nothing -> NoReturn
        Just _  -> Return "r"
    return (MkFunctionSignature { fsName, fsArgs, fsReturn }, nonEmptyConstraints)
  shrink (a, b) = map (a, ) $ shrink b

instance {-# OVERLAPPING #-} Arbitrary (Maybe [FunctionDomainInOutConstraints]) where
  arbitrary = do
    size <- elements [1 .. maxItemsSize]
    oneof
      [ return Nothing
      , do
        constraints <- vectorOf size arbitrary
        return $ Just constraints
      ]

instance {-# OVERLAPPING #-} Arbitrary [(Int, [FunctionDomainInOutConstraints])] where
  arbitrary = do
    size             <- elements [0 .. maxItemsSize]
    inputConstraints <- vectorOf size $ do
      constraint <- vectorOf size arbitrary
      pos        <- choose (0, size)
      return (1, constraint)
    return $ filter (\(_, c) -> not $ null c) inputConstraints

maxArgsLength inputConstraints =
  if null inputConstraints then 0 else fst $ maximumBy (\(l, _) (r, _) -> compare l r) inputConstraints

instance Arbitrary FunctionDomainConstraint where
  arbitrary = do
    size             <- elements [0 .. maxItemsSize]
    inputConstraints <- vectorOf size $ do
      constraint <- vectorOf size arbitrary
      pos        <- choose (0, size)
      return (1, constraint)
    let input = M.fromList inputConstraints
    outputSize <- choose (1, size + 1)
    outputArb  <- vectorOf outputSize arbitrary
    let output = if null outputArb then Nothing else Just outputArb
    return MkFunctionDomainConstraint { input, output }

instance Arbitrary FunctionDomainInOutConstraints where
  arbitrary = do
    let from = MkNegInfinite
    let to   = MkPosInfinite
    return MkRangedFunctionDomainInOutConstraint { FDD.from, FDD.to }

testParseFunctionDomainDirectives :: Text -> FunctionDomainDirectives
testParseFunctionDomainDirectives body =
  let
    parsed = parse "" body functionDomainDirectivesParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, FunctionDomainConstraint)])
  in M.fromList $ map rItem $ extractRight $ extractRight eitherResult
