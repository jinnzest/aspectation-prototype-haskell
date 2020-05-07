module FunctionDomainAspectIO
  ( applyFunctionDomainAspect
  ) where

import AspectsRegister
  ( DirectivesRegister
  , AnalyticsRegister
  , functionDomainDirectives
  , _functionDomainDirectives
  , valueSizeAnalytics
  , resourcesManagementDirectives
  )
import Control.Monad.Except (ExceptT(ExceptT))
import Errors (Errors)
import SemanticTree (SemanticTree, FunctionSignature, FunctionHash)
import FunctionDomainDirectivesIO
  (readFunctionDomainDirectives, writeFunctionDomainDirectivesWithDefaults, propagateFunctionDomainDirectives)
import Control.Lens.Setter (set)
import Aspects (generateMissingDirectives)
import FunctionDomainDirectives (defaultFunctionDomainDirective, embeddedFunctionDomainDirectives)
import FunctionDomainAnalyticsIO
  (readFunctionDomainAnalytics, writeFunctionDomainAnalytics, generateFunctionDomainAnalytics)
import Control.Lens.Getter (view)
import FunctionDomainAnalyticsData
  ( undefinedProbabilities
  , FunctionDomainAnalytics(_constraints, _undefinedProbabilities, MkFunctionDomainAnalytics)
  , constraints
  , UndefinedProbabilityAnalytics
  )
import Data.Map (union, Map, empty)
import FunctionDomainAnalytics (embeddedFunctionDomainAnalytics)
import AspectsShared (applyAspect)
import ValueSizeAnalyticsIO (generateValueSizeAnalytics, writeValueSizeAnalytics)
import ValueSizeAnalytics (embeddedValueSizeAnalytics)
import SemanticTree as Sem (FunctionSignature)
import MemoryManagementDirectivesIO (readMemoryManagementDirectives)
import FunctionDomainDirectivesData (FunctionDomainDirectives)
import AspectsData (EmptyMap)
import Debug.Trace (trace)

applyFunctionDomainAspect
  :: String
  -> SemanticTree
  -> Map Sem.FunctionSignature [Sem.FunctionSignature]
  -> ExceptT Errors IO (FunctionDomainDirectives, UndefinedProbabilityAnalytics)
applyFunctionDomainAspect dir coreTree oldToNewNames = applyAspect
  "function domain"
  dir
  coreTree
  empty
  oldToNewNames
  readDirectives
  writeDirectivesWithDefaults
  propagateDirectives
  defaultDirectivesGenerator
  readAnalytics
  writeAnalytics
  analyticsGenerator

readDirectives :: String -> ExceptT Errors IO FunctionDomainDirectives
readDirectives dir = do
  fdDirectives <- readFunctionDomainDirectives dir
  return $ embeddedFunctionDomainDirectives `union` fdDirectives

writeDirectivesWithDefaults :: String -> FunctionDomainDirectives -> SemanticTree -> IO ()
writeDirectivesWithDefaults dir directives tree = writeFunctionDomainDirectivesWithDefaults dir directives

defaultDirectivesGenerator :: EmptyMap -> FunctionDomainDirectives -> SemanticTree -> FunctionDomainDirectives
defaultDirectivesGenerator _ directives tree = generateMissingDirectives tree defaultFunctionDomainDirective directives

propagateDirectives :: FunctionDomainDirectives -> SemanticTree -> ExceptT Errors IO FunctionDomainDirectives
propagateDirectives directives tree = do
  propagateFunctionDomainDirectives directives tree

readAnalytics :: String -> ExceptT Errors IO UndefinedProbabilityAnalytics
readAnalytics dir = do
  readFunctionDomainAnalytics dir

writeAnalytics :: String -> UndefinedProbabilityAnalytics -> SemanticTree -> IO ()
writeAnalytics dir analytics tree = do
  writeFunctionDomainAnalytics dir tree (embeddedFunctionDomainAnalytics `union` analytics)

analyticsGenerator
  :: EmptyMap
  -> UndefinedProbabilityAnalytics
  -> FunctionDomainDirectives
  -> SemanticTree
  -> ExceptT Errors IO UndefinedProbabilityAnalytics
analyticsGenerator emptyMap analytics directives tree = do
  generateFunctionDomainAnalytics tree directives analytics
