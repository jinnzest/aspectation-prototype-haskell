{-# LANGUAGE TemplateHaskell #-}

module AspectsRegister
  ( DirectivesRegister
    ( MkDirectivesRegister
    , _algorithmicComplexityDirectives
    , _completeAbilityDirectives
    , _functionDomainDirectives
    , _lazinessDirectives
    , _memoryComplexityDirectives
    , _resourcesManagementDirectives
    , _sideEffectDirectives
    , _whenToRunDirectives
    )
  , AnalyticsRegister
    ( MkAnalyticsRegister
    , _execFlowAnalytics
    , _algorithmicComplexityAnalytics
    , _completeAbilityAnalytics
    , _dataTracingAnalytics
    , _undefinedProbabilityAnalytics
    , _valueSizeAnalytics
    , _lazinessAnalytics
    , _resourcesManagementAnalytics
    , _memoryComplexityAnalytics
    , _sideEffectAnalytics
    , _whenToRunAnalytics
    )
  , algorithmicComplexityAnalytics
  , completeAbilityAnalytics
  , dataTracingAnalytics
  , undefinedProbabilityAnalytics
  , lazinessAnalytics
  , valueSizeAnalytics
  , memoryComplexityAnalytics
  , resourcesManagementDirectives
  , resourcesManagementAnalytics
  , sideEffectAnalytics
  , whenToRunAnalytics
  , functionDomainDirectives
  , execFlowAnalytics
  ) where

import AlgorithmicComplexityAnalytics (AlgorithmicComplexityAnalytics)
import AlgorithmicComplexityDirectives (AlgorithmicComplexityDirectives)
import CompleteAbilityAnalytics (CompleteAbilityAnalytics)
import CompleteAbilityDirectives (CompleteAbilityDirectives)
import Control.Lens.TH (makeLenses)
import Data.Map (Map)
import DataTracingAnalytics (DataTracingAnalytics)
import LazinessAnalytics (LazinessAnalytics)
import LazinessDirectives (LazinessDirectives)
import MemoryComplexityAnalytics (MemoryComplexityAnalytics)
import MemoryComplexityDirectives (MemoryComplexityDirectives)
import SideEffectAnalytics (SideEffectAnalytics)
import SideEffectDirectives (SideEffectDirectives)
import WhenToRunAnalytics (WhenToRunAnalytics)
import WhenToRunDirectives (WhenToRunDirectives)
import ValueSizeData (ValueSizeAnalytics)
import MemoryManagementDirectivesData (MemoryManagementDirectives)
import MemoryManagementAnalyticsData (MemoryManagementAnalytics)
import FunctionDomainDirectivesData (FunctionDomainDirectives)
import FunctionDomainAnalyticsData (UndefinedProbabilityAnalytics)
import ExecFlowAnalyticsData (ExecFlowAnalytics)

data DirectivesRegister = MkDirectivesRegister
  { _algorithmicComplexityDirectives :: AlgorithmicComplexityDirectives
  , _completeAbilityDirectives       :: CompleteAbilityDirectives
  , _functionDomainDirectives        :: FunctionDomainDirectives
  , _lazinessDirectives              :: LazinessDirectives
  , _memoryComplexityDirectives      :: MemoryComplexityDirectives
  , _resourcesManagementDirectives   :: MemoryManagementDirectives
  , _sideEffectDirectives            :: SideEffectDirectives
  , _whenToRunDirectives             :: WhenToRunDirectives
  }

data AnalyticsRegister = MkAnalyticsRegister
  { _algorithmicComplexityAnalytics :: AlgorithmicComplexityAnalytics
  , _completeAbilityAnalytics       :: CompleteAbilityAnalytics
  , _dataTracingAnalytics           :: DataTracingAnalytics
  , _execFlowAnalytics              :: ExecFlowAnalytics
  , _undefinedProbabilityAnalytics  :: UndefinedProbabilityAnalytics
  , _valueSizeAnalytics             :: ValueSizeAnalytics
  , _lazinessAnalytics              :: LazinessAnalytics
  , _memoryComplexityAnalytics      :: MemoryComplexityAnalytics
  , _resourcesManagementAnalytics   :: MemoryManagementAnalytics
  , _sideEffectAnalytics            :: SideEffectAnalytics
  , _whenToRunAnalytics             :: WhenToRunAnalytics
  }

$(makeLenses ''AnalyticsRegister)

$(makeLenses ''DirectivesRegister)
