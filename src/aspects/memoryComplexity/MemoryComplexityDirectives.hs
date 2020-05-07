module MemoryComplexityDirectives
  ( MemoryComplexityDirectiveVal
  , MemoryComplexityDirectives
  , readMemoryComplexityDirectives
  , defaultMemoryComplexityDirective
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, String, ($), return)

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree (Arg, Return)

data MemoryComplexityDirectiveVal
  = OC
  | Any

type MemoryComplexityDirectives = Directives MemoryComplexityDirectiveVal

readMemoryComplexityDirectives :: String -> ExceptT Errors IO MemoryComplexityDirectives
readMemoryComplexityDirectives dir = do
  liftIO $ putStrLn "Reading memory complexity directives..."
  return empty

defaultMemoryComplexityDirective :: [Text] -> Maybe Text -> MemoryComplexityDirectiveVal
defaultMemoryComplexityDirective args ret = Any
