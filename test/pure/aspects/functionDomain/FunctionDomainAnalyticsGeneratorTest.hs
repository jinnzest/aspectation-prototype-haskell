module FunctionDomainAnalyticsGeneratorTest
  ( functionDomainAnalyticsGeneratorTest
  ) where

import SemanticTreeArbitrary (maxItemsSize, mkRanged)
import AspectsTestArbitrary ()
import FunctionDomainTestShared (testParseFunctionDomainDirectives)
import Control.Monad.Except (ExceptT, Except, mapExceptT, runExcept, runExceptT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map as M (Map, empty, fromList, lookup, member, toList)
import Data.List (sortBy)
import Data.Text (Text, append, pack, unpack)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, elements, oneof, shrink, testProperty, vectorOf, suchThat, choose)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Pos (SourcePos(SourcePos), mkPos, sourceColumn, sourceLine, sourceName)

import AspectsData as Asp (Analytics, Directives, FunctionCall(MkFunctionCall, fcName, fcArgs))
import Errors (Error(MkError), Errors(MkErrors))
import FunctionDomainAnalytics (generateFunctionDomainAnalytics)
import FunctionDomainAnalyticsData
  ( FunctionDomainAnalytics(MkFunctionDomainAnalytics, _undefinedProbabilities)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, callProbability, functionCall)
  , UndefinedProbabilityAnalytics
  , UndefinedFunctionProbability(MkUndefinedFunctionProbability)
  , UndefinedProbability(MaybeDefined, Defined)
  , resultUndefinedProbability
  , callUndefinedProbabilities
  , CallProbability(MaybeWillBeCalled, WillBeCalled)
  )
import FunctionDomainDirectives (propagateFunctionDomainDirectives)
import FunctionDomainDirectivesData as FDD
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainConstraintFrom(MkNegInfinite)
  , FunctionDomainConstraintTo(MkPosInfinite)
  , from
  , input
  , output
  , to
  )
import FunctionDomainAnalyticsParser (functionDomainAnalyticsParser)
import Location as Loc (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import Panic (panic)
import ParserWrapper (parse)
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName, tmpVarName)
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
import TestShared (parseStep, testParse, setOf)
import FunctionDomainAnalyticsWriters (writeFunctionDomainAnalyticsMap)
import AspectsData (FunctionCall)
import AspectsTestShared (extractRight)
import Data.Set as S (fromList)
import FunctionDomainArbitrary ()

embeddedFunctions =
  [Sem.MkFunctionSignature { Sem.fsName = "print", Sem.fsArgs = [Sem.MkValue "arg"], Sem.fsReturn = Sem.NoReturn }]

functionDomainAnalyticsGeneratorTest :: TestTree
functionDomainAnalyticsGeneratorTest = testGroup
  "Function domain analytics tests"
  [ testCase "function is defined when caller domain is indefinite and sub function call is indefinite too"
    $ let
        directives = testParseFunctionDomainDirectives
          "main arg ~ -inf<=arg<=inf\nprint v ~ -inf<=v<=inf\nfunc arg ~ -inf<=arg<=inf"
        tree                            = testParse "fn main arg = func arg\nfn func arg = print arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities  = testParseUndefinedProbabilities
          "main arg ~ \n\t1 func arg ~ will be called\nfunc arg ~ \n\t1 print arg ~ will be called"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  , testCase "function is defined when caller domain is finite and sub function call is indefinite"
    $ let
        directives = testParseFunctionDomainDirectives
          "main arg ~ -100<=arg<=10000000000000000000\nprint v ~ -inf<=v<=inf\nfunc arg ~  -inf<=arg<=inf"
        tree                            = testParse "fn main arg = func arg\nfn func arg = print arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities  = testParseUndefinedProbabilities
          "main arg ~ \n\t1 func arg ~ will be called\nfunc arg ~ \n\t1 print arg ~ will be called"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  , testCase "function is defined  when caller domain is finite and sub function call is defined"
    $ let
        directives = testParseFunctionDomainDirectives
          "main arg ~ -inf<=arg<=inf\nprint v ~ -inf<=v<=inf\nfunc arg ~  -100<=arg<=10000000000000000000"
        tree                            = testParse "fn main arg = func arg\nfn func arg = print arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities  = testParseUndefinedProbabilities
          "main arg ~ \n\t1 func arg ~ maybe will be called\nfunc arg ~ \n\t1 print arg ~ will be called"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  , testCase
      "function is defined when caller domain is finite and sub function call is indefinite and returning arg as result"
    $ let
        directives = testParseFunctionDomainDirectives
          "main arg -> r ~ -100<=arg<=10000000000000000000\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -inf<=arg<=inf"
        tree                            = testParse "fn main arg = func arg\nfn func arg = arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities  = testParseUndefinedProbabilities
          "main arg -> r ~ r = defined\n\t1 a <- func arg ~ will be called;a=defined\nfunc arg -> r ~ r = defined"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  , testCase
      "function is maybe defined when caller domain is indefinite  and sub function call is finite and returning arg as result"
    $ let
        directives = testParseFunctionDomainDirectives
          "main arg -> r ~ -inf<=arg<=inf\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -100 <=arg<=10000000000000000000"
        tree                             = testParse "fn main arg = func arg\nfn func arg = arg"
        generatedFunctionDomainAnalytics = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities =
          testParseUndefinedProbabilities
            "main arg -> r ~ r = maybe defined\n\t1 a <- func arg ~ maybe will be called;a = maybe defined\nfunc arg -> r ~ r = defined"
      in assertEqual "" expectedUndefinedProbabilities generatedFunctionDomainAnalytics
  , testCase
      "function is maybe defined when caller domain for a sub call is indefinite and arg of the sub call is maybe defined result of another subcall because latter the arg is finite"
    $ let
        directives =
          testParseFunctionDomainDirectives
            "main arg -> r ~ -inf<=arg<=inf\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -inf <=arg<=inf\nfunc2 arg -> r ~ -10<=arg<=10"
        tree = testParse "fn main arg = func func2 arg\nfn func arg = arg\nfn func2 arg = arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities =
          testParseUndefinedProbabilities
            "main arg -> r ~ r = maybe defined\n|\t1 a <- func func2 arg\n\t1 a <- func2 arg ~ maybe will be called;a = maybe defined\n\t1 a <- func arg ~ maybe will be called; a= maybe defined\nfunc2 arg -> r ~ r = defined\nfunc arg -> r ~ r = defined"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  , testCase
      "function is maybe defined when caller domain is from -100 to 100 but sub caller functions from -indefinite to 10 and from -10 to indefinite"
    $ let
        directives =
          testParseFunctionDomainDirectives
            "main arg -> r ~ -100<=arg<=100\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -inf <=arg<=10\nfunc2 arg -> r ~ -10<=arg<=inf"
        tree = testParse "fn main arg = \n\tfunc arg\n\tfunc2 arg\nfn func arg = arg\nfn func2 arg = arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities =
          testParseUndefinedProbabilities
            "main arg -> r ~ r = maybe defined\n\t1 func arg ~ maybe will be called\n\t2 a <- func2 arg ~ maybe will be called; a = maybe defined\nfunc arg -> r ~ r = defined\nfunc2 arg -> r ~ r = defined"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  , testCase
      "function is  defined when caller domain is from -10 to 10 but sub caller functions from -indefinite to 10 and from -10 to indefinite"
    $ let
        directives =
          testParseFunctionDomainDirectives
            "main arg -> r ~ -10<=arg<=10\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -inf <=arg<=10\nfunc2 arg -> r ~ -10<=arg<=inf"
        tree = testParse "fn main arg = \n\tfunc arg\n\tfunc2 arg\nfn func arg = arg\nfn func2 arg = arg"
        generatedUndefinedProbabilities = generateFunctionDomainAnalytics embeddedFunctions tree directives M.empty
        expectedUndefinedProbabilities =
          testParseUndefinedProbabilities
            "main arg -> r ~ r = defined\n\t1 func arg ~ will be called\n\t2 a <- func2 arg ~ will be called; a = defined\nfunc arg -> r ~ r = defined\nfunc2 arg -> r ~ r = defined"
      in assertEqual "" expectedUndefinedProbabilities generatedUndefinedProbabilities
  ]

functionsToCallEachOther :: [Function] -> [Function]
functionsToCallEachOther functions = map
  (\f -> MkFunction
    { fSignature = fSignature f
    , fBody      =
      map
          (\f2 ->
            let sig = fSignature f2
            in
              mkRanged $ MkNoAssignment $ Sem.MkFunctionCall
                { fcsName    = mkRanged $ Sem.fsName sig
                , fcsArgs    = zipWith
                  (\p a -> MkFunctionArgument Nothing (mkRanged p))
                  [0 .. (length $ Sem.fsArgs sig)]
                  (Sem.fsArgs sig)
                , tmpVarName = Nothing
                }
          )
        $ filter (\f3 -> fSignature f3 /= fSignature f) functions
    }
  )
  functions

testParseAspect body =
  let
    parsed = parse "" body functionDomainAnalyticsParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, UndefinedFunctionProbability)])
  in M.fromList $ map rItem $ extractRight body $ extractRight body eitherResult

testParseUndefinedProbabilities :: Text -> Map FunctionSignature UndefinedFunctionProbability
testParseUndefinedProbabilities body =
  let
    parsed = parse "" body functionDomainAnalyticsParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, UndefinedFunctionProbability)])
  in M.fromList $ map rItem $ extractRight body $ extractRight body eitherResult

extractUndefinedError :: Except Errors FunctionDomainAnalytics -> Errors
extractUndefinedError result =
  let
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) result) :: Either Errors (Either Errors FunctionDomainAnalytics)
  in
    case eitherResult of
      Right res -> case res of
        Left  err -> err
        Right r   -> panic ("Unexpected: " ++ show r)
      Left unexpected -> panic ("Unexpected: " ++ show unexpected)


