module CompleteAbilityAnalytics
  ( CompleteAbilityAnalytics
  , readCompleteAbilityAnalytics
  , writeCompleteAbilityAnalytics
  , generateCompleteAbilityAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn)
import Prelude (IO, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)
import Text.Show (Show)

data CompleteAbilityAnalyticsVal = AlwaysComplete
  deriving Show

type CompleteAbilityAnalytics = Analytics CompleteAbilityAnalyticsVal

readCompleteAbilityAnalytics :: ExceptT Errors IO CompleteAbilityAnalytics
readCompleteAbilityAnalytics = do
  liftIO $ putStrLn "Reading complete ability analytics..."
  return empty

generateCompleteAbilityAnalytics :: SemanticTree -> ExceptT Errors IO CompleteAbilityAnalytics
generateCompleteAbilityAnalytics coreTree = do
  liftIO $ putStrLn "Generating complete ability analytics..."
  return empty

writeCompleteAbilityAnalytics :: CompleteAbilityAnalytics -> IO ()
writeCompleteAbilityAnalytics analytics = do
  liftIO $ putStrLn "Writing complete ability analytics..."
  return ()
