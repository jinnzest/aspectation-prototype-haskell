module MainHashesIO
  ( writeMainHashesIO
  , readMainHashesIO
  ) where

import Control.Monad.Except (void)
import Data.Bifunctor (bimap, first)
import Data.List (sortBy)
import Data.Text as T (Text, append, concat, null, pack)
import Data.Text.IO as TIO (putStrLn, readFile, writeFile)
import Data.Void (Void)
import Location (rItem)
import Prelude as P
  (Bool(False, True), Either(Left, Right), IO, String, ($), (++), (.), (<*), (==), const, foldl, map, null, return)

import MainHashes (parseHashesSyntax, writeMainHashes)
import SemanticTree as Sem
  ( Arg(MkValue)
  , FunctionSignature
  , FunctionSignature(MkFunctionSignature)
  , NameArgs(MkNameArgs)
  , FunctionHash
  , FunctionHash(MkFunctionHash)
  , fsArgs
  , fsName
  , fsReturn
  , functionHash
  , naArgs
  , naName
  )
import SyntaxParser (equals, functionArgs, functionName)
import SyntaxToSemantic (synSignatureToNameArgs)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.Megaparsec (Parsec, eof, getSourcePos, parse, some)
import Text.Megaparsec.Char (alphaNumChar)
import AspectsShared (generatedDirWith)
import Debug.Trace (trace)
import Text.Show (Show(show))
import Data.Map (Map, empty)

type Parser = Parsec Void Text

hashesDirWith :: String -> String -> String
hashesDirWith path = generatedDirWith ("hashes/main/" ++ path)
hashesDir :: String -> String
hashesDir = hashesDirWith ""

writeMainHashesIO :: String -> Map Sem.FunctionSignature FunctionHash -> IO ()
writeMainHashesIO dir hashesSignatures = do
  TIO.putStrLn "Writing main hashes..."
  let path          = hashesDirWith "main.hsh" dir
  let hashesDirPath = hashesDir dir
  createDirectoryIfMissing True hashesDirPath
  let body = writeMainHashes hashesSignatures
  TIO.writeFile path body

readMainHashesIO :: String -> IO (Map Sem.FunctionSignature FunctionHash)
readMainHashesIO dir = do
  TIO.putStrLn "Reading main language hashes..."
  createDirectoryIfMissing True $ hashesDir dir
  let path = hashesDirWith "main.hsh" dir
  fileExists <- doesFileExist path
  if fileExists then readFileHashes path else return empty

readFileHashes :: String -> IO (Map Sem.FunctionSignature FunctionHash)
readFileHashes path = do
  body <- TIO.readFile path
  parseSyntaxIO path body

parseSyntaxIO :: String -> Text -> IO (Map Sem.FunctionSignature FunctionHash)
parseSyntaxIO filePath fileBody = return $ parseHashesSyntax filePath fileBody
