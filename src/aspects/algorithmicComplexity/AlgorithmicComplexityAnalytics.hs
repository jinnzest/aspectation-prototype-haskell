module AlgorithmicComplexityAnalytics
  ( AlgorithmicComplexityAnalytics
  , AlgorithmicComplexityAnalyticsVal
  , readAlgorithmicComplexityAnalytics
  , writeAlgorithmicComplexityAnalytics
  , generateAlgorithmicComplexityAnalytics
  ) where

import Prelude ()
import Control.Monad.Except (ExceptT, liftIO)
import Data.Text.IO (putStrLn)
import Text.Show (Show)
import Data.Function (($))
import System.IO (IO)
import Control.Monad (Monad(return))
import Data.Map (empty)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree (SemanticTree)

data AlgorithmicComplexityAnalyticsVal = OC
  deriving Show

type AlgorithmicComplexityAnalytics = Analytics AlgorithmicComplexityAnalyticsVal

readAlgorithmicComplexityAnalytics :: ExceptT Errors IO AlgorithmicComplexityAnalytics
readAlgorithmicComplexityAnalytics = do
  liftIO $ putStrLn "Reading algorithmic complexity analytics..."
  return empty

generateAlgorithmicComplexityAnalytics
  :: AlgorithmicComplexityAnalytics -> SemanticTree -> ExceptT Errors IO AlgorithmicComplexityAnalytics
generateAlgorithmicComplexityAnalytics analytics coreTree = do
  liftIO $ putStrLn "Generating algorithmic complexity analytics..."
  return empty

writeAlgorithmicComplexityAnalytics :: AlgorithmicComplexityAnalytics -> IO ()
writeAlgorithmicComplexityAnalytics analytics = do
  liftIO $ putStrLn "Writing algorithmic complexity analytics..."
  return ()
