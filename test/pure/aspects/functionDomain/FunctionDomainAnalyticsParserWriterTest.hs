module FunctionDomainAnalyticsParserWriterTest
  ( functionDomainAnalyticsParserWriterTest
  ) where

import SemanticTreeArbitrary (mkRanged)
import AspectsTestArbitrary ()
import FunctionDomainTestShared (testParseFunctionDomainAnalytics)
import Control.Monad.Except (ExceptT, Except, runExceptT, mapExceptT)
import Data.Functor.Identity (Identity)
import Data.Map as M (fromList, toList, Map, empty)
import Data.Text (Text, pack)
import Data.Maybe (isNothing)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, testProperty, suchThat, oneof, choose)
import Control.Monad.Identity (Identity(runIdentity))
import Panic (panic)
import Text.Megaparsec.Pos (SourcePos(SourcePos, sourceName, sourceLine, sourceColumn), mkPos)
import AspectsTestShared (genSemanticTree, maxItemsSize)
import AspectsData (Analytics, Directives, FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName), FunctionCall)
import Errors (Error(MkError), Errors(MkErrors))
import FunctionDomainAnalytics (generateFunctionDomainAnalytics)
import FunctionDomainAnalyticsIO (readFunctionDomainAnalytics, writeFunctionDomainAnalytics)
import FunctionDomainDirectives (propagateFunctionDomainDirectives)
import FunctionDomainAnalyticsData
  ( UndefinedFunctionProbability(MkUndefinedFunctionProbability)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, callProbability, functionCall)
  , callUndefinedProbabilities
  , resultUndefinedProbability
  , UndefinedProbabilityAnalytics
  , UndefinedProbability(MaybeDefined, Defined)
  , CallProbability(MaybeWillBeCalled, WillBeCalled)
  )
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
import FunctionDomainDirectivesParser (functionDomainDirectivesParser)
import Location as Loc (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import ParserWrapper (parse)
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , NameArgs(MkNameArgs, naArgs, naName)
  , Return(NoReturn, Return)
  , Return(NoReturn, Return)
  , FunctionHash(MkFunctionHash)
  , SemanticModel(MkSemanticModel)
  , SemanticTree(MkSemanticTree)
  , Statement(MkNoAssignment, expression)
  , fsArgs
  , fsName
  , fsReturn
  )
import ShowJ (showJ)
import TestShared (parseStep, testParse)
import FunctionDomainAnalyticsParser (functionDomainAnalyticsParser)
import FunctionDomainAnalyticsWriters (writeFunctionDomainAnalyticsMap)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.List (groupBy, sort, sortBy)
import Data.Set as S (empty, fromList, toList, union)
import FunctionDomainArbitrary ()

functionDomainAnalyticsParserWriterTest :: TestTree
functionDomainAnalyticsParserWriterTest = testGroup
  "Function domain analytics parser and writer tests"
  [ testCase "write function with a function result being passed as an argument to another function"
    $ let
        main    = testParse "fn f1 a = a\nfn f2 a= a\nfn main =\n\tf1 1\n\tf1 f2 2\n\tf2 2"
        written = writeFunctionDomainAnalyticsMap
          main
          (M.fromList
            [ ( MkFunctionSignature { fsName = "f1", fsArgs = [MkValue "a"], fsReturn = Return "r" }
              , MkUndefinedFunctionProbability
                { callUndefinedProbabilities = S.empty
                , resultUndefinedProbability = Just Defined
                }
              )
            , ( MkFunctionSignature { fsName = "f2", fsArgs = [MkValue "a"], fsReturn = Return "r" }
              , MkUndefinedFunctionProbability
                { callUndefinedProbabilities = S.empty
                , resultUndefinedProbability = Just Defined
                }
              )
            , ( MkFunctionSignature { fsName = "main", fsArgs = [], fsReturn = Return "r" }
              , MkUndefinedFunctionProbability
                { callUndefinedProbabilities = S.fromList
                  [ MkPositionedFunctionCall
                    { callPos       = 1
                    , functionCalls = S.fromList
                      [ MkFunctionCallWithProbability
                          { callProbability = WillBeCalled
                          , functionCall    = MkFunctionCall { fcName = "f1", fcArgs = ["a"], tmpVarName = Nothing }
                          }
                      ]
                    }
                  , MkPositionedFunctionCall
                    { callPos       = 2
                    , functionCalls = S.fromList
                      [ MkFunctionCallWithProbability
                        { callProbability = WillBeCalled
                        , functionCall    = MkFunctionCall { fcName = "f1", fcArgs = ["a"], tmpVarName = Nothing }
                        }
                      , MkFunctionCallWithProbability
                        { callProbability = WillBeCalled
                        , functionCall    = MkFunctionCall { fcName = "f2", fcArgs = ["a"], tmpVarName = Nothing }
                        }
                      ]
                    }
                  , MkPositionedFunctionCall
                    { callPos       = 3
                    , functionCalls = S.fromList
                      [ MkFunctionCallWithProbability
                          { callProbability = WillBeCalled
                          , functionCall    = MkFunctionCall { fcName = "f2", fcArgs = ["a"], tmpVarName = Nothing }
                          }
                      ]
                    }
                  ]
                , resultUndefinedProbability = Just Defined
                }
              )
            ]
          )
      in assertEqual
        ""
        "f1 a -> r ~ r= defined\n|\t1 a\n\nf2 a -> r ~ r= defined\n|\t1 a\n\nmain -> r ~ r= defined\n\t1 f1 a ~ will be called\n|\t2 f1 f2 2\n\t2 f1 a ~ will be called\n\t2 f2 a ~ will be called\n\t3 f2 a ~ will be called\n"
        written
  , testProperty "read the same function undefined probabilities as were written" $ \writtenConstraints ->
    let
      writtenUndefinedProbabilities = M.fromList writtenConstraints
      body                          = writeFunctionDomainAnalyticsMap
        (genSemanticTree findMaxPos writtenUndefinedProbabilities)
        writtenUndefinedProbabilities
      readUndefinedProbabilities           = testParseFunctionDomainAnalytics body
      groupedWrittenUndefinedProbabilities = groupByCallPos writtenUndefinedProbabilities
    in
      (groupedWrittenUndefinedProbabilities == readUndefinedProbabilities)
        || trace
             (  "\n\nbody: "
             ++ show body
             ++ "\n\ngroupedWrittenUndefinedProbabilities: "
             ++ show groupedWrittenUndefinedProbabilities
             ++ "\n\nreadUndefinedProbabilities:    "
             ++ show readUndefinedProbabilities
             )
             False
  ]

groupByCallPos writtenUndefinedProbabilities =
  M.fromList
    $ map
        (\(s, p) ->
          ( s
          , MkUndefinedFunctionProbability
            { callUndefinedProbabilities =
              S.fromList
              $ map
                  (\l -> MkPositionedFunctionCall
                    { callPos       = callPos $head l
                    , functionCalls = S.fromList $concat $ map (S.toList . functionCalls) l
                    }
                  )
              $ groupBy (\l r -> callPos l == callPos r)
              $ S.toList $callUndefinedProbabilities p
            , resultUndefinedProbability = resultUndefinedProbability p
            }
          )
        )
    $ M.toList writtenUndefinedProbabilities

findMaxPos a =
  let calls = S.toList $ callUndefinedProbabilities a
  in let positions = map callPos calls in if null positions then 0 else maximum positions
