module LazinessDirectives
  ( LazinessDirectiveVal
  , LazinessDirectives
  , readLazinessDirectives
  , defaultLazinessDirective
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, String, ($), return)

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree (Arg, Return)

data LazinessDirectiveVal = Strict

type LazinessDirectives = Directives LazinessDirectiveVal

readLazinessDirectives :: String -> ExceptT Errors IO LazinessDirectives
readLazinessDirectives dir = do
  liftIO $ putStrLn "Reading laziness directives..."
  return empty

defaultLazinessDirective :: [Text] -> Maybe Text -> LazinessDirectiveVal
defaultLazinessDirective args ret = Strict
