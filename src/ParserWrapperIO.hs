module ParserWrapperIO
  ( parsePath
  ) where

import Control.Monad.Except (Except, ExceptT, (>>=), liftIO, mapExceptT, return, throwError)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map as M (Map, empty, fromList)
import Data.Text (Text, append, pack)
import Data.Text.IO as TIO (putStrLn, readFile)
import Prelude
  (Either(Left, Right), IO, Maybe(Just), Show, String, ($), (++), (.), fst, fst, map, map, not, not, return, show)

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
import ParserWrapper (parse)

parsePath :: String -> Text -> Parser [p] -> ExceptT Errors IO [p]
parsePath filePath message parser = do
  liftIO $ putStrLn $ append "\n" message
  parseFile filePath parser

parseFile :: String -> Parser [p] -> ExceptT Errors IO [p]
parseFile filePath parser = do
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then do
      fileBody <- liftIO $ readFile filePath
      mapExceptT (return . runIdentity) $ parse filePath fileBody parser
    else return []

