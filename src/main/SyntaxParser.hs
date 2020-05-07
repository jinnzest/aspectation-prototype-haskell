module SyntaxParser
  ( Parser
  , parseSyntax
  , functionArgs
  , functionName
  , functionSignature
  , equals
  , parseSkipNewLine
  , integer
  , keyword
  , identifier
  , assign
  , lexemeSc
  , ranged
  , returnName
  , isAbout
  ) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (void, Monad(return))
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing))
import Data.Text (Text, concat, pack, singleton)
import Data.Text.IO (putStrLn, readFile)
import Data.Void (Void)
import GHC.Types (Char)
import Prelude ()
import Text.Megaparsec (Parsec, (<|>), empty, eof, errorBundlePretty, getSourcePos, many, optional, parse, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer (IndentOpt(IndentSome), decimal, indentBlock, lexeme, nonIndented, space)

import Environment (getWorkingDir)
import Location (Range(MkRange), Ranged(MkRanged), from, rItem, range, to)
import SemanticTree (SemanticTree, SemanticTree)
import SyntaxTree
  ( Expression(MkIdentifier, MkInteger)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature)
  , Statement(MkNoAssignment)
  , fsArgs
  , fsName
  )
import CorePrelude (Integer, Num((*)))
import Data.Function (($), (.))
import Data.List (head, last)

type Parser = Parsec Void Text

parseSyntax = functions

functions :: Parser [Ranged Function]
functions = some function <* eof

function :: Parser (Ranged Function)
function = try singleLineFunc <|> multiLineFunc

parseSkipNewLine :: Parser ()
parseSkipNewLine = void (space space1 empty empty)

skipSpaces :: Parser ()
skipSpaces = space (void $ some $ char ' ' <|> char '\t') empty empty

lexemeSc :: Parser a -> Parser a
lexemeSc = lexeme skipSpaces

multiLineFunc :: Parser (Ranged Function)
multiLineFunc = do
  from                          <- getSourcePos
  (fsName, from, fsArgs, fBody) <- nonIndented parseSkipNewLine $ indentBlock parseSkipNewLine $ do
    from <- getSourcePos
    fn
    MkFunctionSignature { fsName = n, fsArgs = args } <- functionSignature
    equals
    return $ IndentSome Nothing (return . (n, from, args, )) statement
  to <- getSourcePos
  let rItem      = MkFunctionSignature { fsName, fsArgs }
  let range      = MkRange { from, to }
  let fSignature = MkRanged { rItem, range }
  let rItem      = (MkFunction { fSignature, fBody })
  return MkRanged { rItem, range = MkRange { from, to } }

singleLineFunc :: Parser (Ranged Function)
singleLineFunc = nonIndented parseSkipNewLine $ do
  from <- getSourcePos
  fn
  rItem <- functionSignature
  sigTo <- getSourcePos
  equals
  statement <- statement
  return $ MkRanged
    { rItem =
      (MkFunction { fSignature = MkRanged { rItem, range = MkRange { from, to = sigTo } }, fBody = [statement] })
    , range = MkRange { from, to = to $ range statement }
    }

functionSignature :: Parser FunctionSignature
functionSignature = do
  fsName <- functionName
  fsArgs <- functionArgs
  return MkFunctionSignature { fsName, fsArgs }

functionArgs :: Parser [Ranged Text]
functionArgs = many functionArg

functionArg :: Parser (Ranged Text)
functionArg = ranged $ lexemeSc $ do
  parsed <- some $ alphaNumChar <|> char '_'
  return $ pack parsed

functionName :: Parser (Ranged Text)
functionName = ranged $ lexemeSc identifier

integer :: Parser Integer
integer = do
  signed <- optional $ lexemeSc $ char '-'
  int    <- lexemeSc decimal
  return $ int * maybe 1 (\_ -> -1) signed

keyword :: Text -> Parser ()
keyword = void . lexemeSc . string

fn :: Parser ()
fn = keyword "fn"

equals :: Parser ()
equals = keyword "="

returnName :: Parser (Maybe Text)
returnName = optional $ try $ do
  void $ lexemeSc $ string "->"
  lexemeSc identifier

isAbout :: Parser ()
isAbout = keyword "~"

identifier :: Parser Text
identifier = do
  head <- letterChar
  tail <- many $ alphaNumChar <|> char '_'
  return $ pack $ head : tail

statement :: Parser (Ranged Statement)
statement = do
  statement <- noAssignment
  void parseSkipNewLine
  return statement

noAssignment :: Parser (Ranged Statement)
noAssignment = do
  expressions <- some expression
  let f     = from $ range $ head expressions
  let t     = to $ range $ last expressions
  let range = MkRange { from = f, to = t }
  let rItem = MkNoAssignment expressions
  return $ MkRanged { rItem, range }

expression :: Parser (Ranged Expression)
expression = do
  expr <- ranged $ MkInteger <$> integer <|> MkIdentifier <$> identifier
  void skipSpaces
  return expr

ranged :: Parser a -> Parser (Ranged a)
ranged itemParser = do
  from  <- getSourcePos
  rItem <- itemParser
  to    <- getSourcePos
  let range = MkRange { from, to }
  return MkRanged { rItem, range }

assign = keyword "<-"
