module MemoryManagementAnalyticsIO
  ( readMemoryManagementAnalytics
  , writeMemoryManagementAnalytics
  , generateMemoryManagementAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn, writeFile)
import Prelude (IO, Maybe, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)
import AspectsShared (analyticsDirWith, analyticsExt)
import Data.List ((++))
import MemoryManagementAnalyticsData (MemoryManagementAnalytics)
-- import MemoryManagementAnalyticsWriters (writeMemoryManagementAnalyticsMap)
import AspectsRegister (DirectivesRegister, AnalyticsRegister)
import MemoryManagementDirectivesData (MemoryManagementDirectives)
import ValueSizeData (ValueSizeAnalytics)
import Data.String (String)
import MemoryManagementAnalyticsWriters (writeMemoryManagementAnalyticsMap)
import qualified MemoryManagementAnalytics as Pure (generateMemoryManagementAnalytics)
import ExecFlowAnalyticsData (ExecFlowAnalytics)

readMemoryManagementAnalytics :: ExceptT Errors IO MemoryManagementAnalytics
readMemoryManagementAnalytics = do
  liftIO $ putStrLn "Reading resources management analytics..."
  return empty

generateMemoryManagementAnalytics
  :: (ValueSizeAnalytics, ExecFlowAnalytics)
  -> MemoryManagementAnalytics
  -> MemoryManagementDirectives
  -> SemanticTree
  -> ExceptT Errors IO MemoryManagementAnalytics
generateMemoryManagementAnalytics valueSizeAndExecFlowAnalytics analytics directives tree = do
  liftIO $ putStrLn "Generating resources management analytics..."
  return $ Pure.generateMemoryManagementAnalytics valueSizeAndExecFlowAnalytics tree directives analytics

writeMemoryManagementAnalytics :: String -> MemoryManagementAnalytics -> SemanticTree -> IO ()
writeMemoryManagementAnalytics dir analytics tree = do
  liftIO $ putStrLn "Writing resources management analytics..."
  let upBody = writeMemoryManagementAnalyticsMap tree analytics
  let upPath = resourcesManagementAnalyticsPath dir
  writeFile upPath upBody
  return ()

resourcesManagementAnalyticsPath :: String -> String
resourcesManagementAnalyticsPath = analyticsDirWith $ "memory-management." ++ analyticsExt
