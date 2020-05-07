module ExecFlowAnalyticsData
  ( ExecFlowAnalytics
  ) where

import AspectsData (Analytics)
import SemanticTree (FunctionSignature)
import ShowJ (showJ)

type ExecFlowAnalytics = Analytics [[FunctionSignature]]

instance   {-# OVERLAPPING #-}  Show ExecFlowAnalytics where
  show = showJ
