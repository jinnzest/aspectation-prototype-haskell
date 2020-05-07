module AspectsParser
  ( parseSemanticFunction
  , parseFuncCall
  , parseSkipNewLine
  , parseInt
  ) where

import Control.Monad (void)
import Data.Text (Text, pack)
import Text.Megaparsec (many, optional, some, try, empty)
import Text.Megaparsec.Char (char, string, space1)

import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import SyntaxParser (Parser, identifier, keyword, lexemeSc, returnName)
import Text.Megaparsec.Char.Lexer (skipLineComment, space, decimal)
import SemanticTree
  (FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn), showArg, Arg(MkValue), Return(NoReturn, Return))
import Text.Megaparsec.Debug (dbg)

parseInt :: Parser Int
parseInt = do
  signed <- optional $ lexemeSc $ char '-'
  int    <- lexemeSc decimal
  return $ int * maybe 1 (\_ -> -1) signed

parseSkipNewLine :: Parser ()
parseSkipNewLine = void (space space1 (skipLineComment "|") empty)

parseSemanticFunction :: Parser FunctionSignature
parseSemanticFunction = do
  fsName <- parseName
  args   <- parseArgs
  ret    <- returnName
  return MkFunctionSignature { fsName, fsArgs = map MkValue args, fsReturn = maybe NoReturn Return ret }

parseFuncCall :: Parser FunctionCall
parseFuncCall = do
  tmpVarName <- -- dbg "tmpVarName" $ 
                optional $ try $ do
    name <- lexemeSc identifier
    keyword "<-"
    return name
  fcName <- lexemeSc identifier
  fcArgs <- many $ do
    lexemeSc identifier
  return MkFunctionCall { fcName, fcArgs, tmpVarName }

parseName :: Parser Text
parseName = lexemeSc identifier

parseArgs :: Parser [Text]
parseArgs = many $ lexemeSc identifier

