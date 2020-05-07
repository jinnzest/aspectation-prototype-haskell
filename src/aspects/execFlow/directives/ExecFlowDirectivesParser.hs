module ExecFlowDirectivesParser
  ( execFlowDirectivesParser
  ) where

import Prelude ()
import Location (Ranged(MkRanged, rItem, range), Range(MkRange))
import ExecFlowDirectivesData (Visibility(Pub, Priv))
import CorePrelude (error)
import SyntaxParser (Parser, equals, keyword, parseSkipNewLine, isAbout)
import Text.Megaparsec (MonadParsec(eof, try), many, getSourcePos, (<|>))
import Control.Applicative (Applicative((<*)))
import Control.Monad (Monad(return))
import qualified Location as L
import AspectsParser (parseSemanticFunction)
import SemanticTree (FunctionSignature)

execFlowDirectivesParser :: Parser [Ranged (FunctionSignature, Visibility)]
execFlowDirectivesParser = many execFlowDirective <* eof

execFlowDirective :: Parser (Ranged (FunctionSignature, Visibility))
execFlowDirective = do
  from <- getSourcePos
  sig  <- parseSemanticFunction
  isAbout
  val <- parseItemValue
  to  <- getSourcePos
  parseSkipNewLine
  return MkRanged { rItem = (sig, val), range = MkRange { L.from, L.to } }

parseItemValue :: Parser Visibility
parseItemValue = try parsePub <|> try parsePriv

parsePub = do
  keyword "pub"
  return Pub

parsePriv = do
  keyword "priv"
  return Priv
