module ExecFlowAnalyticsIO
  ( readAnalytics
  , writeAnalytics
  ) where

import Prelude ()
import Control.Monad.Except (ExceptT, MonadIO(liftIO))
import Errors (Errors)
import ExecFlowAnalyticsData (ExecFlowAnalytics)
import SemanticTree (SemanticTree)
import Data.Map (empty)
import ExecFlowAnalyticsWriter (writeExecFlowAnalytics)
import AspectsShared (analyticsDirWith, analyticsExt)
import Data.Text.IO (putStrLn, writeFile)
import Data.String (String)
import System.IO (IO)
import Control.Monad (Monad(return))
import Data.Function (($))
import Data.List ((++))

readAnalytics :: String -> ExceptT Errors IO ExecFlowAnalytics
readAnalytics dir = return empty

writeAnalytics :: String -> ExecFlowAnalytics -> SemanticTree -> IO ()
writeAnalytics dir analytics _ = do
  liftIO $ putStrLn "Writing execution flow analytics..."
  let upBody = writeExecFlowAnalytics analytics
  let upPath = execFlowAnalyticsFilePath dir
  writeFile upPath upBody
  return ()


execFlowAnalyticsFilePath = analyticsDirWith $ "exec-flow." ++ analyticsExt
