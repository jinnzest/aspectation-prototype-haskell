module FunctionDomainTestShared
  ( testParseFunctionDomainAnalytics
  , testParseFunctionDomainDirectives
  ) where

import Data.Text (Text)
import Data.Map (fromList, Map)
import SemanticTree (FunctionSignature)
import FunctionDomainAnalyticsData (UndefinedFunctionProbability)
import FunctionDomainDirectivesData (FunctionDomainConstraint)
import Errors (Errors)
import Location as Loc (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import ParserWrapper (parse)
import FunctionDomainAnalyticsParser (functionDomainAnalyticsParser)
import FunctionDomainDirectivesParser (functionDomainDirectivesParser)
import Control.Monad.Except (ExceptT, Except, runExceptT, mapExceptT)
import Control.Monad.Identity (Identity(runIdentity))
import Panic (panic)

extractRight _    (Right r) = r
extractRight body other     = panic ("\nunexpected: " ++ show other ++ "\nbody: " ++ show body)

testParseFunctionDomainAnalytics :: Text -> Map FunctionSignature UndefinedFunctionProbability
testParseFunctionDomainAnalytics body =
  let
    parsed = parse "" body functionDomainAnalyticsParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, UndefinedFunctionProbability)])
  in fromList $ map rItem $ extractRight body $ extractRight body eitherResult

testParseFunctionDomainDirectives :: Text -> Map FunctionSignature FunctionDomainConstraint
testParseFunctionDomainDirectives body =
  let
    parsed = parse "" body functionDomainDirectivesParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, FunctionDomainConstraint)])
  in fromList $ map rItem $ extractRight body $ extractRight body eitherResult
