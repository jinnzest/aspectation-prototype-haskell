module LazinessAnalytics
  ( LazinessAnalytics
  , readLazinessAnalytics
  , writeLazinessAnalytics
  , generateLazinessAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn)
import Prelude (IO, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)

data LazinessAnalyticsVal = Strict

type LazinessAnalytics = Analytics LazinessAnalyticsVal

readLazinessAnalytics :: ExceptT Errors IO LazinessAnalytics
readLazinessAnalytics = do
  liftIO $ putStrLn "Reading laziness analytics..."
  return empty

generateLazinessAnalytics :: SemanticTree -> ExceptT Errors IO LazinessAnalytics
generateLazinessAnalytics coreTree = do
  liftIO $ putStrLn "Generating laziness analytics..."
  return empty

writeLazinessAnalytics :: LazinessAnalytics -> IO ()
writeLazinessAnalytics analytics = do
  liftIO $ putStrLn "Writing laziness analytics..."
  return ()
