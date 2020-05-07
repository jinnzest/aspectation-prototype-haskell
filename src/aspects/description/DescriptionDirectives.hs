module DescriptionDirectives
  ( DescriptionDirectives
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude (IO, ($), return)

import AspectsData (Directives)
import Errors (Errors)

newtype DescriptionDirectiveVal =
  MkText Text

type DescriptionDirectives = Directives DescriptionDirectiveVal

readDescriptionDirectives :: ExceptT Errors IO DescriptionDirectives
readDescriptionDirectives = do
  liftIO $ putStrLn "Reading description directives..."
  return empty

writeDescriptionDirectives :: DescriptionDirectives -> IO ()
writeDescriptionDirectives directives = do
  liftIO $ putStrLn "Writing description directives..."
  return ()
