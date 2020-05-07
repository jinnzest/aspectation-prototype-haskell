module ParserWrapper
  ( ParserWrapper.parse
  ) where

import Control.Monad.Except (Except, ExceptT, (>>=), liftIO, mapExceptT, return, throwError)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map as M (Map, empty, fromList)
import Data.Text (Text, append, pack)
import Data.Text.IO as TIO (putStr, putStrLn, readFile)
import Text.Megaparsec as MP (bundleErrors, bundlePosState, parse, pstateSourcePos)

import AspectsData as Sem (Analytics)
import Control.Monad (unless)
import Errors (Error(MkError), Errors(MkErrors))
import Location (Range(MkRange), Ranged(MkRanged), from, rItem, range, to)
import SyntaxParser (Parser, parseSyntax)
import SyntaxToSemantic (synSignatureToNameArgs)
import SyntaxTree (FunctionSignature, SyntaxTree)
import System.Directory (doesFileExist)
import System.IO (IOMode(ReadWriteMode), hClose, openFile)
import Text.Megaparsec.Error (ParseError(TrivialError), errorBundlePretty)

parse :: String -> Text -> Parser [p] -> ExceptT Errors Identity [p]
parse filePath fileBody parser = case MP.parse parser filePath fileBody of
  Left  bundle -> throwError $ MkErrors [MkError $ pack $ errorBundlePretty bundle]
  Right res    -> return res
