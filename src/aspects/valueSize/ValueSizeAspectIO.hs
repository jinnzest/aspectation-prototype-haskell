module ValueSizeAspectIO
  ( applyValueSizeAspect
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
import Control.Lens.Setter (set)
import Aspects (generateMissingDirectives)
import Control.Lens.Getter (view)
import Data.Map (union, Map, empty)
import AspectsShared (applyAspect)
import ValueSizeAnalyticsIO (generateValueSizeAnalytics, writeValueSizeAnalytics)
import ValueSizeAnalytics (embeddedValueSizeAnalytics)
import SemanticTree as Sem (FunctionSignature)
import MemoryManagementDirectivesIO (readMemoryManagementDirectives)
import ValueSizeData (ValueSizeAnalytics)
import AspectsData (Directives, EmptyMap)
import FunctionDomainDirectivesData (FunctionDomainDirectives)


applyValueSizeAspect
  :: String
  -> SemanticTree
  -> FunctionDomainDirectives
  -> Map Sem.FunctionSignature [Sem.FunctionSignature]
  -> ExceptT Errors IO (EmptyMap, ValueSizeAnalytics)
applyValueSizeAspect dir coreTree functionDomainDirectives oldToNewNames = applyAspect
  "value size"
  dir
  coreTree
  functionDomainDirectives
  oldToNewNames
  readDirectives
  writeDirectivesWithDefaults
  propagateDirectives
  defaultDirectivesGenerator
  readAnalytics
  writeAnalytics
  analyticsGenerator

readDirectives :: String -> ExceptT Errors IO EmptyMap
readDirectives dir = return empty

writeDirectivesWithDefaults :: String -> EmptyMap -> SemanticTree -> IO ()
writeDirectivesWithDefaults dir register tree = return ()

defaultDirectivesGenerator :: FunctionDomainDirectives -> EmptyMap -> SemanticTree -> EmptyMap
defaultDirectivesGenerator _ _ _ = empty

propagateDirectives :: EmptyMap -> SemanticTree -> ExceptT Errors IO EmptyMap
propagateDirectives _ _ = return empty

readAnalytics :: String -> ExceptT Errors IO ValueSizeAnalytics
readAnalytics _ = return empty

writeAnalytics :: String -> ValueSizeAnalytics -> SemanticTree -> IO ()
writeAnalytics dir analytics tree = do
  writeValueSizeAnalytics dir (embeddedValueSizeAnalytics `union` analytics) tree

analyticsGenerator
  :: FunctionDomainDirectives -> ValueSizeAnalytics -> EmptyMap -> SemanticTree -> ExceptT Errors IO ValueSizeAnalytics
analyticsGenerator functionDomainDirectives analytics directives tree = do
  generateValueSizeAnalytics tree functionDomainDirectives analytics
