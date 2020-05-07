module SyntaxParserIO
  ( parseSyntaxIO
  , getWorkingDir
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Data.Text (Text, pack)
import Prelude (Either(Left, Right), IO, String, ($), (.), return, show)
import Text.Megaparsec (bundleErrors, bundlePosState, parse, pstateSourcePos)

import Environment (getWorkingDir)
import Errors (Error(MkRangedError), Errors(MkErrors))
import Location (Range(MkRange), Ranged(MkRanged), from, rItem, range, to)
import ParserWrapperIO (parsePath)
import SemanticTree (SemanticTree)
import SyntaxParser (parseSyntax)
import SyntaxTree (SyntaxTree)

parseSyntaxIO :: String -> Text -> ExceptT Errors IO SyntaxTree
parseSyntaxIO filePath fileBody = let message = "Parsing main language" in parsePath filePath message parseSyntax
