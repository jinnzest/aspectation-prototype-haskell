module CompleteAbilityDirectives
  ( CompleteAbilityDirectiveVal
  , CompleteAbilityDirectives
  , readCompleteAbilityDirectives
  , defaultCompleteAbilityDirective
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, String, ($), return)

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree (Arg, Return)

data CompleteAbilityDirectiveVal
  = AlwaysComplete
  | Any

type CompleteAbilityDirectives = Directives CompleteAbilityDirectiveVal

readCompleteAbilityDirectives :: String -> ExceptT Errors IO CompleteAbilityDirectives
readCompleteAbilityDirectives dir = do
  liftIO $ putStrLn "Reading complete ability directives..."
  return empty

defaultCompleteAbilityDirective :: [Text] -> Maybe Text -> CompleteAbilityDirectiveVal
defaultCompleteAbilityDirective args ret = Any
