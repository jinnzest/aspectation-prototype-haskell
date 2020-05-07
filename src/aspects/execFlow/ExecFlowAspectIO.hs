module ExecFlowAspectIO
  ( applyExecFLowAspect
  ) where

import Prelude ()
import AspectsRegister
  ( DirectivesRegister
  , AnalyticsRegister
  , functionDomainDirectives
  , _functionDomainDirectives
  , valueSizeAnalytics
  , resourcesManagementDirectives
  )
import Control.Monad.Except (ExceptT(ExceptT), MonadIO(liftIO))
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
import AspectsShared (applyAspect, analyticsDirWith, analyticsExt)
import ValueSizeAnalyticsIO (generateValueSizeAnalytics, writeValueSizeAnalytics)
import ValueSizeAnalytics (embeddedValueSizeAnalytics)
import qualified SemanticTree as Sem
import MemoryManagementDirectivesIO (readMemoryManagementDirectives)
import FunctionDomainDirectivesData (FunctionDomainDirectives)
import AspectsData (EmptyMap)
import ExecFlowAnalyticsData (ExecFlowAnalytics)
import ExecFlowAnalyticsWriter (writeExecFlowAnalytics)
import Data.Text.IO (putStrLn, writeFile)
import System.IO (IO)
import Debug.Trace (trace)
import Data.String (String)
import Control.Monad (Monad(return))
import Data.Function (const, ($))
import Data.List ((++), map)
import Text.Show (Show(show))
import ExecFlowDirectivesData (ExecFlowDirectives)
import ExecFlowDirectivesIO (writeExecFlowDirectivesWithDefaults, readExecFlowDirectives)
import ExecFlowDirectives (defaultExecFlowDirective)
import ExecFlowAnalytics (generateExecFlowAnalytics)
import qualified Data.Map as M
import Data.Tuple (fst)
import ExecFlowAnalyticsIO (readAnalytics, writeAnalytics)

applyExecFLowAspect
  :: String
  -> SemanticTree
  -> Map Sem.FunctionSignature [Sem.FunctionSignature]
  -> ExceptT Errors IO (ExecFlowDirectives, ExecFlowAnalytics)
applyExecFLowAspect dir coreTree oldToNewNames = applyAspect
  "exec flow"
  dir
  coreTree
  empty
  oldToNewNames
  readExecFlowDirectives
  writeDirectivesWithDefaults
  (\d _ -> return d)
  defaultDirectivesGenerator
  readAnalytics
  writeAnalytics
  analyticsGenerator

writeDirectivesWithDefaults :: String -> ExecFlowDirectives -> SemanticTree -> IO ()
writeDirectivesWithDefaults dir directives tree = writeExecFlowDirectivesWithDefaults dir directives

defaultDirectivesGenerator :: EmptyMap -> ExecFlowDirectives -> SemanticTree -> ExecFlowDirectives
defaultDirectivesGenerator _ directives tree = generateMissingDirectives tree defaultExecFlowDirective directives

analyticsGenerator
  :: EmptyMap -> ExecFlowAnalytics -> ExecFlowDirectives -> SemanticTree -> ExceptT Errors IO ExecFlowAnalytics
analyticsGenerator _ analytics directives tree = return $ generateExecFlowAnalytics tree directives
