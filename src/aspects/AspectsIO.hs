module AspectsIO
  ( applyAspects
  ) where

import Prelude ()
import Data.String (String)
import SemanticTree (FunctionHash, SemanticTree, FunctionSignature)
import Control.Monad.Except (ExceptT(ExceptT), MonadIO(liftIO))
import Errors (Errors)
import System.IO (IO)
import AspectsRegister
  ( DirectivesRegister
    ( MkDirectivesRegister
    , _functionDomainDirectives
    , _algorithmicComplexityDirectives
    , _completeAbilityDirectives
    , _lazinessDirectives
    , _memoryComplexityDirectives
    , _resourcesManagementDirectives
    , _sideEffectDirectives
    , _whenToRunDirectives
    )
  , AnalyticsRegister
    ( MkAnalyticsRegister
    , _undefinedProbabilityAnalytics
    , _algorithmicComplexityAnalytics
    , _completeAbilityAnalytics
    , _dataTracingAnalytics
    , _valueSizeAnalytics
    , _lazinessAnalytics
    , _memoryComplexityAnalytics
    , _resourcesManagementAnalytics
    , _sideEffectAnalytics
    , _whenToRunAnalytics
    , _execFlowAnalytics
    )
  )
import Data.Function (($))
import Data.Text.IO (putStrLn)
import Control.Monad (Monad(return))
import System.Directory (createDirectoryIfMissing)
import Data.Bool (Bool(True))
import AspectsShared
  ( directivesWithDefaultsDirWith
  , analyticsDirWith
  , directivesDirWith
  , directivesDir
  , directivesWithDefaultsDir
  , analyticsDir
  )
import Data.List ((++))
import Data.Map (Map, empty)
import SemanticTree as Sem (FunctionSignature)
import ValueSizeAspectIO (applyValueSizeAspect)
import FunctionDomainAspectIO (applyFunctionDomainAspect)
import MemoryManagementAspectIO (applyMemoryManagementAspect)
import ExecFlowAspectIO (applyExecFLowAspect)

applyAspects
  :: String
  -> Map Sem.FunctionSignature [Sem.FunctionSignature]
  -> SemanticTree
  -> ExceptT Errors IO (DirectivesRegister, AnalyticsRegister)
applyAspects dir oldToNewNames coreTree = do
  liftIO $ putStrLn "\nApplying aspects..."
  liftIO $ createAspectDirs dir
  (_functionDomainDirectives, _undefinedProbabilityAnalytics) <- applyFunctionDomainAspect dir coreTree oldToNewNames
  (_, _valueSizeAnalytics) <- applyValueSizeAspect dir coreTree _functionDomainDirectives oldToNewNames
  (_, _execFlowAnalytics) <- applyExecFLowAspect dir coreTree oldToNewNames
  (_resourcesManagementDirectives, _resourcesManagementAnalytics) <- applyMemoryManagementAspect
    dir
    coreTree
    (_valueSizeAnalytics, _execFlowAnalytics)
    oldToNewNames
  return
    ( MkDirectivesRegister
      { _algorithmicComplexityDirectives = empty
      , _completeAbilityDirectives       = empty
      , _functionDomainDirectives
      , _lazinessDirectives              = empty
      , _memoryComplexityDirectives      = empty
      , _resourcesManagementDirectives
      , _sideEffectDirectives            = empty
      , _whenToRunDirectives             = empty
      }
    , MkAnalyticsRegister
      { _algorithmicComplexityAnalytics = empty
      , _completeAbilityAnalytics       = empty
      , _dataTracingAnalytics           = empty
      , _execFlowAnalytics
      , _undefinedProbabilityAnalytics
      , _valueSizeAnalytics
      , _lazinessAnalytics              = empty
      , _memoryComplexityAnalytics      = empty
      , _resourcesManagementAnalytics
      , _sideEffectAnalytics            = empty
      , _whenToRunAnalytics             = empty
      }
    )

createAspectDirs dir = do
  createDirectoryIfMissing True $ directivesWithDefaultsDir dir
  createDirectoryIfMissing True $ analyticsDir dir
