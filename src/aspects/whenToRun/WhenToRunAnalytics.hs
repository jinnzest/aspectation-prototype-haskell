module WhenToRunAnalytics
  ( WhenToRunAnalytics
  , readWhenToRunAnalytics
  , writeWhenToRunAnalytics
  , generateWhenToRunAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn)
import Prelude (IO, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)

data WhenToRunAnalyticsVal = Runtime

type WhenToRunAnalytics = Analytics WhenToRunAnalyticsVal

readWhenToRunAnalytics :: ExceptT Errors IO WhenToRunAnalytics
readWhenToRunAnalytics = do
  liftIO $ putStrLn "Reading when to run analytics..."
  return empty

generateWhenToRunAnalytics :: SemanticTree -> ExceptT Errors IO WhenToRunAnalytics
generateWhenToRunAnalytics coreTree = do
  liftIO $ putStrLn "Generating when to run analytics..."
  return empty

writeWhenToRunAnalytics :: WhenToRunAnalytics -> IO ()
writeWhenToRunAnalytics analytics = do
  liftIO $ putStrLn "Writing when to run analytics..."
  return ()
