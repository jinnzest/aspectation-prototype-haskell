module MemoryComplexityAnalytics
  ( MemoryComplexityAnalytics
  , readMemoryComplexityAnalytics
  , writeMemoryComplexityAnalytics
  , generateMemoryComplexityAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)

data MemoryComplexityAnalyticsVal = OC

type MemoryComplexityAnalytics = Analytics MemoryComplexityAnalyticsVal

readMemoryComplexityAnalytics :: ExceptT Errors IO MemoryComplexityAnalytics
readMemoryComplexityAnalytics = do
  liftIO $ putStrLn "Reading memory complexity analytics..."
  return empty

generateMemoryComplexityAnalytics :: SemanticTree -> ExceptT Errors IO MemoryComplexityAnalytics
generateMemoryComplexityAnalytics coreTree = do
  liftIO $ putStrLn "Generating memory complexity analytics..."
  return empty

writeMemoryComplexityAnalytics :: MemoryComplexityAnalytics -> IO ()
writeMemoryComplexityAnalytics analytics = do
  liftIO $ putStrLn "Writing memory complexity analytics..."
  return ()
