module DataTracingAnalytics
  ( DataTracingAnalytics
  , readDataTracingAnalytics
  , writeDataTracingAnalytics
  , generateDataTracingAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)
import Text.Show (Show)

newtype DataTracingAnalyticsVal =
  DataTracingAnalyticsVal Text deriving Show

type DataTracingAnalytics = Analytics DataTracingAnalyticsVal

readDataTracingAnalytics :: ExceptT Errors IO DataTracingAnalytics
readDataTracingAnalytics = do
  liftIO $ putStrLn "Reading data tracing analytics..."
  return empty

generateDataTracingAnalytics :: SemanticTree -> ExceptT Errors IO DataTracingAnalytics
generateDataTracingAnalytics coreTree = do
  liftIO $ putStrLn "Generating data tracing analytics..."
  return empty

writeDataTracingAnalytics :: DataTracingAnalytics -> IO ()
writeDataTracingAnalytics analytics = do
  liftIO $ putStrLn "Writing data tracing analytics..."
  return ()
