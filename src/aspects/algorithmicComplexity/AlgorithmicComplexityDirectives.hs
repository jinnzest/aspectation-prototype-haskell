module AlgorithmicComplexityDirectives
  ( AlgorithmicComplexityDirectives
  , AlgorithmicComplexityDirectiveVal(OC, Any)
  , readAlgorithmicComplexityDirectives
  , defaultAlgorithmicComplexityDirective
  ) where

import Prelude ()
import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (empty, lookup, toList)
import Data.Maybe (Maybe, fromJust)
import Data.Text as T (Text, pack, unpack)
import Data.Text.IO as TIO (putStrLn, readFile, writeFile)
import Text.Show (Show)
import Data.Eq (Eq)
import Data.String (String)
import System.IO (IO)
import Data.Function (($))
import Control.Monad (Monad(return))

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree (Arg, Return, SemanticTree, fSignature, FunctionSignature, Function)

data AlgorithmicComplexityDirectiveVal
  = OC
  | Any
  deriving (Show, Eq)

type AlgorithmicComplexityDirectives = Directives AlgorithmicComplexityDirectiveVal

readAlgorithmicComplexityDirectives :: String -> ExceptT Errors IO AlgorithmicComplexityDirectives
readAlgorithmicComplexityDirectives dir = do
  liftIO $ putStrLn "Reading algorithmic complexity directives..."
  return empty

defaultAlgorithmicComplexityDirective :: Function -> AlgorithmicComplexityDirectiveVal
defaultAlgorithmicComplexityDirective _ = Any
