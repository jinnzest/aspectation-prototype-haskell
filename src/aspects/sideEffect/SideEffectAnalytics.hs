module SideEffectAnalytics
  ( SideEffectAnalytics
  , readSideEffectAnalytics
  , writeSideEffectAnalytics
  , generateSideEffectAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn)
import Prelude (IO, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)

data SideEffectAnalyticsVal = No

type SideEffectAnalytics = Analytics SideEffectAnalyticsVal

readSideEffectAnalytics :: ExceptT Errors IO SideEffectAnalytics
readSideEffectAnalytics = do
  liftIO $ putStrLn "Reading side effect analytics..."
  return empty

generateSideEffectAnalytics :: SemanticTree -> ExceptT Errors IO SideEffectAnalytics
generateSideEffectAnalytics coreTree = do
  liftIO $ putStrLn "Generating side effect analytics..."
  return empty

writeSideEffectAnalytics :: SideEffectAnalytics -> IO ()
writeSideEffectAnalytics analytics = do
  liftIO $ putStrLn "Writing side effect analytics..."
  return ()
