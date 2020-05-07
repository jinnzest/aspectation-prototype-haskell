module AspectsParserIO
  ( parseAspectsOnPath
  ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, (>>=), liftIO, return, throwError)
import Data.Map as M (Map, empty, fromList)
import Data.Text (Text, append, pack)
import Data.Text.IO as TIO (putStr, putStrLn, readFile)
import Text.Megaparsec (bundleErrors, bundlePosState, parse, pstateSourcePos)

import Errors (Error(MkRangedError), Errors(MkErrors))
import Location (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import ParserWrapperIO (parsePath)
import SemanticTree (SemanticTree, FunctionSignature)
import SyntaxParser (Parser, parseSyntax)
import SyntaxToSemantic (synSignatureToNameArgs)
import SyntaxTree (SyntaxTree)
import System.Directory (doesFileExist)
import System.IO (IOMode(ReadWriteMode), hClose, openFile)

parseAspectsOnPath
  :: String -> Text -> Parser [Ranged (FunctionSignature, v)] -> ExceptT Errors IO (Map FunctionSignature v)
parseAspectsOnPath filePath message parser = do
  parsed <- parsePath filePath message parser
  return $ M.fromList $ map rItem parsed
