module ExecFlowAnalyticsGeneratorTest
  ( execFlowAnalyticsGeneratorTest
  ) where

import Data.List as Lst (map)
import Test.Tasty
import Test.Tasty.HUnit (testCase, assertEqual)
import Data.Text (Text)
import Data.Map (Map, empty)
import ExecFlowAnalyticsData (ExecFlowAnalytics)
import SemanticTree (FunctionSignature)
import Errors (Errors)
import Location (Ranged(rItem))
import Data.Map as M
import ParserWrapper (parse)
import ExecFlowAnalyticsParser (execFlowAnalyticsParser)
import Control.Monad.Except (runExceptT, mapExceptT)
import Control.Monad.Identity (Identity(runIdentity))
import AspectsTestShared (extractRight)
import TestShared (testParse)
import ExecFlowAnalytics (generateExecFlowAnalytics)
import ExecFlowDirectivesParser (execFlowDirectivesParser)
import ExecFlowDirectivesData (ExecFlowDirectives, Visibility)
import Debug.Trace (trace)

execFlowAnalyticsGeneratorTest :: TestTree
execFlowAnalyticsGeneratorTest = testGroup
  "Execution flow analytics tests"
  [ testCase "a function calling a function only once"
    $ let
        analytics                  = testParseExecFlowAnalytics "main arg -> r ~ \n\tfunc v -> r"
        tree                       = testParse "fn main arg = \n\tfunc arg\nfn func arg = arg"
        directives                 = testParseExecFlowDirectives "main arg -> r ~ pub\nfunc arg -> r ~ priv"
        generatedExecFlowAnalytics = generateExecFlowAnalytics tree directives
      in assertEqual "" analytics generatedExecFlowAnalytics
  , testCase "a function calling a function twice"
    $ let
        analytics  = testParseExecFlowAnalytics "main a -> r ~ \n\tfunc v -> r"
        tree       = testParse "fn main arg = \n\tfunc arg\n\tfunc 2\nfn func arg = arg"
        directives = testParseExecFlowDirectives "main arg -> r ~ pub\nfunc arg -> r ~ priv"
        generatedExecFlowAnalytics = -- trace ("\ntree: " ++ show tree) 
          generateExecFlowAnalytics tree directives
      in assertEqual "" analytics generatedExecFlowAnalytics
  , testCase "a function calling two functions"
    $ let
        analytics  = testParseExecFlowAnalytics "main a -> r ~ \n\tfunc v -> r\n\tfunc2 a b -> r"
        tree       = testParse "fn main arg = \n\tfunc arg\n\tfunc2 4 arg\nfn func arg = arg\nfn func2 a b = b"
        directives = testParseExecFlowDirectives "main arg -> r ~ pub\nfunc arg -> r ~ priv\nfunc2 a b -> r ~ priv"
        generatedExecFlowAnalytics = -- trace ("\ntree: " ++ show tree) 
          generateExecFlowAnalytics tree directives
      in assertEqual "" analytics generatedExecFlowAnalytics
  , testCase "a function calling three functions hierarchy"
    $ let
        analytics = testParseExecFlowAnalytics "main arg -> r ~ \n\tfunc1 v -> r , func2 v v -> r, func3 a -> r"
        tree =
          testParse "fn main arg = \n\tfunc1 arg\nfn func1 arg = func2 arg arg\nfn func2 a b = func3 b\nfn func3 a = a"
        directives = testParseExecFlowDirectives
          "main arg -> r ~ pub\nfunc1 arg -> r ~ priv\nfunc2 a b -> r ~ priv\nfunc3 a -> r ~ priv"
        generatedExecFlowAnalytics = -- trace ("\ntree: " ++ show tree) 
          generateExecFlowAnalytics tree directives
      in assertEqual "" analytics generatedExecFlowAnalytics
  , testCase "a function calling three functions hierarchy with duplicating calls"
    $ let
        analytics =
          testParseExecFlowAnalytics
            "main arg -> r ~ \n\tfunc1 v -> r , func2 v v -> r, func3 a -> r\n\tfunc2 v v -> r, func3 a -> r\n\tfunc3 a -> r"
        tree =
          testParse
            "fn main arg = \n\tfunc1 arg\n\tfunc2 arg arg\n\tfunc3 arg\nfn func1 arg = func2 arg arg\nfn func2 a b = func3 b\nfn func3 a = a"
        directives = testParseExecFlowDirectives
          "main arg -> r ~ pub\nfunc1 arg -> r ~ priv\nfunc2 a b -> r ~ priv\nfunc3 a -> r ~ priv"
        generatedExecFlowAnalytics = -- trace ("\ntree: " ++ show tree) 
          generateExecFlowAnalytics tree directives
      in assertEqual "" analytics generatedExecFlowAnalytics
  ]

testParseExecFlowDirectives :: Text -> ExecFlowDirectives
testParseExecFlowDirectives body =
  let
    parsed = parse "" body execFlowDirectivesParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, Visibility)])
  in M.fromList $ Lst.map rItem $ extractRight body $ extractRight body eitherResult

testParseExecFlowAnalytics :: Text -> ExecFlowAnalytics
testParseExecFlowAnalytics body =
  let
    parsed = parse "" body execFlowAnalyticsParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, [[FunctionSignature]])])
  in M.fromList $ Lst.map rItem $ extractRight body $ extractRight body eitherResult
