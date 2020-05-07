module MemoryManagementAspectIO
  ( applyMemoryManagementAspect
  ) where

import Prelude ()
import Data.String (String)
import SemanticTree
  (SemanticTree(MkSemanticTree), FunctionSignature, FunctionHash, extractFunctions, Function(fSignature))
import Control.Monad.Except (ExceptT(ExceptT))
import Errors (Errors)
import System.IO (IO)
import AspectsRegister
  ( DirectivesRegister
  , AnalyticsRegister
  , _resourcesManagementDirectives
  , resourcesManagementDirectives
  , resourcesManagementAnalytics
  , valueSizeAnalytics
  )
import AspectsShared (applyAspect)
import Control.Monad (Monad(return))
import MemoryManagementDirectivesIO (writeMemoryManagementDirectivesWithDefaults, readMemoryManagementDirectives)
import Data.Function (($))
import Control.Lens.Setter (set)
import Aspects (generateMissingDirectives)
import MemoryManagementAnalyticsIO
  (readMemoryManagementAnalytics, generateMemoryManagementAnalytics, writeMemoryManagementAnalytics)
import Control.Lens.Getter (view)
import Data.Map as M (Map, empty, union, fromList)
import SemanticTree as Sem (FunctionSignature)
import MemoryManagementDirectivesData (MemoryManagementDirectives)
import MemoryManagementAnalyticsData (MemoryManagementAnalytics)
import AspectsData (EmptyMap)
import MemoryManagementDirectives (defaultMemoryManagementDirective)
import ValueSizeData (ValueSizeAnalytics)
import Data.List as L (concat, map)
import ExecFlowAnalyticsData (ExecFlowAnalytics)

applyMemoryManagementAspect
  :: String
  -> SemanticTree
  -> (ValueSizeAnalytics, ExecFlowAnalytics)
  -> Map Sem.FunctionSignature [Sem.FunctionSignature]
  -> ExceptT Errors IO (MemoryManagementDirectives, MemoryManagementAnalytics)
applyMemoryManagementAspect dir coreTree valueSizeAnalytics oldToNewNames = applyAspect
  "resources management"
  dir
  coreTree
  valueSizeAnalytics
  oldToNewNames
  readDirectives
  writeDirectivesWithDefaults
  propagateDirectives
  defaultDirectivesGenerator
  readAnalytics
  writeMemoryManagementAnalytics
  generateMemoryManagementAnalytics

readDirectives :: String -> ExceptT Errors IO MemoryManagementDirectives
readDirectives = readMemoryManagementDirectives

writeDirectivesWithDefaults :: String -> MemoryManagementDirectives -> SemanticTree -> IO ()
writeDirectivesWithDefaults dir directives tree = writeMemoryManagementDirectivesWithDefaults dir tree directives

defaultDirectivesGenerator
  :: (ValueSizeAnalytics, ExecFlowAnalytics) -> MemoryManagementDirectives -> SemanticTree -> MemoryManagementDirectives
defaultDirectivesGenerator (valueSizeAnalytics, _) directives tree =
  let
    functions          = extractFunctions tree
    functionSignatures = M.fromList $ L.map (\f -> (fSignature f, fSignature f)) functions
  in generateMissingDirectives tree (defaultMemoryManagementDirective valueSizeAnalytics functionSignatures) directives

propagateDirectives :: MemoryManagementDirectives -> SemanticTree -> ExceptT Errors IO MemoryManagementDirectives
propagateDirectives register tree = return register

readAnalytics :: String -> ExceptT Errors IO MemoryManagementAnalytics
readAnalytics dir = return empty--do
  -- analytics <- readMemoryManagementAnalytics dir
  -- return $ set resourcesManagementAnalytics analytics register
