module SideEffectDirectives
  ( SideEffectDirectiveVal
  , SideEffectDirectives
  , readSideEffectDirectives
  , defaultSideEffectDirective
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, String, ($), return)

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree (Arg, Return)

data SideEffectDirectiveVal
  = No
  | Any

type SideEffectDirectives = Directives SideEffectDirectiveVal

readSideEffectDirectives :: String -> ExceptT Errors IO SideEffectDirectives
readSideEffectDirectives dir = do
  liftIO $ putStrLn "Reading side effects directives..."
  return empty

defaultSideEffectDirective :: [Text] -> Maybe Text -> SideEffectDirectiveVal
defaultSideEffectDirective args ret = Any
