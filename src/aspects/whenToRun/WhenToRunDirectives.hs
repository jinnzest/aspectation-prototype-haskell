module WhenToRunDirectives
  ( WhenToRunDirectiveVal
  , WhenToRunDirectives
  , readWhenToRunDirectives
  , defaultWhenToRunDirective
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, String, ($), return)

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree (Arg, Return)

data WhenToRunDirectiveVal = Runtime

type WhenToRunDirectives = Directives WhenToRunDirectiveVal

readWhenToRunDirectives :: String -> ExceptT Errors IO WhenToRunDirectives
readWhenToRunDirectives dir = do
  liftIO $ putStrLn "Reading when to run directives..."
  return empty

defaultWhenToRunDirective :: [Text] -> Maybe Text -> WhenToRunDirectiveVal
defaultWhenToRunDirective args ret = Runtime
