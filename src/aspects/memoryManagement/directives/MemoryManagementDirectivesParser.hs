module MemoryManagementDirectivesParser
  ( resourcesManagementDirectives
  ) where

import Prelude (Num(fromInteger), error)
import Location as L (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import Control.Monad (fail, void, when, Monad(return), mapM_)
import AspectsData (FunctionCall(fcArgs))
import FunctionDomainDirectivesData (FunctionDomainConstraint)
import Data.List as List (groupBy, intercalate, length, filter, map, (++), head, null, concat, foldl, reverse, sortBy)
import Data.Map as M (fromList, lookup)
import Text.Megaparsec (Parsec, (<|>), eof, getSourcePos, many, optional, some, try)
import Control.Applicative (Applicative((<*)), (<$>))
import AspectsParser (parseSemanticFunction, parseSemanticFunction, parseSkipNewLine)
import SyntaxParser (Parser, assign, equals, identifier, integer, lexemeSc, keyword, isAbout)
import Data.Text (Text, pack)
import MemoryManagementDirectivesData as RMD
  ( StatementMemoryManagementDirectives(MkStatementMemoryManagementDirectives, statementPos, statementMemoryManagement)
  , ExpressionMemoryManagementDirectives
    ( MkCallMemoryManagementDirectives
    , argumentsManagement
    , functionSignature
    , returnManagement
    )
  , MemoryManagementDirective(AllocHere, AllocAbove)
  )
import Text.Megaparsec.Char.Lexer (nonIndented, indentBlock, IndentOpt(IndentMany))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Nothing, Just), isJust)
import Text.Megaparsec.Char (char)
import Data.Map (Map)
import Data.Int (Int)
import Data.Tuple (swap)
import Data.List.Index (indexed)
import Data.List as Lst ((++), map, foldl, head, groupBy)
import Shared (justLookup)
import Maybes (fromJust)
import Data.Bifunctor (Bifunctor(first))
import Data.Eq (Eq((==)))
import Data.Ord (Ord(compare))
import Debug.Trace (trace)
import Text.Show (Show(show))
import Text.Megaparsec.Debug (dbg)
import SemanticTree (FunctionSignature(fsArgs, fsReturn), Arg(MkValue), Return(Return, NoReturn), showArg, unpackArg)
import Data.Bool (Bool(False), Bool(True))


groupByCallPos :: [StatementMemoryManagementDirectives] -> [StatementMemoryManagementDirectives]
groupByCallPos ungroupedLineCallsMemoryManagementDirectives =
  let
    groupedLineCallsMemoryManagementDirectives =
      groupBy (\left right -> statementPos left == statementPos right) ungroupedLineCallsMemoryManagementDirectives
    lineCallsMemoryManagementDirectives = List.map
      (\a -> MkStatementMemoryManagementDirectives
        { statementPos              = statementPos $ head a
        , statementMemoryManagement = foldl (\acc curr -> acc ++ statementMemoryManagement curr) [] a
        }
      )
      groupedLineCallsMemoryManagementDirectives
  in lineCallsMemoryManagementDirectives

resourcesManagementDirectives :: Parser [Ranged (FunctionSignature, [StatementMemoryManagementDirectives])]
resourcesManagementDirectives = many resourcesManagementDirectivesItem <* eof

resourcesManagementDirectivesItem :: Parser (Ranged (FunctionSignature, [StatementMemoryManagementDirectives]))
resourcesManagementDirectivesItem = do
  from <- getSourcePos
  (sig, unGroupedLineCallsMemoryManagementDirectives) <-
    nonIndented parseSkipNewLine $ indentBlock parseSkipNewLine $ do
      sig <- parseSemanticFunction
      isAbout
      return $ IndentMany Nothing (return . (sig, )) $ parseItemValue $ Lst.map unpackArg $fsArgs sig
  to <- getSourcePos
  let groupedCallMemoryManagementDirectives = groupByCallPos unGroupedLineCallsMemoryManagementDirectives
  return MkRanged { rItem = (sig, groupedCallMemoryManagementDirectives), range = MkRange { L.from, L.to } }

parseItemValue :: [Text] -> Parser StatementMemoryManagementDirectives
parseItemValue args = do
  statementPos              <- lexemeSc integer
  statementMemoryManagement <- parseCallMemoryManagementDirectives
  return $ MkStatementMemoryManagementDirectives { statementPos = fromInteger statementPos, statementMemoryManagement }

parseCallMemoryManagementDirectives :: Parser [ExpressionMemoryManagementDirectives]
parseCallMemoryManagementDirectives = do
  funcCall      <- parseCallMemoryManagementDirectiveItem
  funcCallsTail <- many $ do
    void $ lexemeSc $ char ';'
    parseCallMemoryManagementDirectiveItem
  return $ funcCall : funcCallsTail

parseCallMemoryManagementDirectiveItem :: Parser ExpressionMemoryManagementDirectives
parseCallMemoryManagementDirectiveItem = do
  functionSignature <- parseSemanticFunction
  let argsMap = M.fromList $ List.map swap $ indexed $ fsArgs functionSignature
  keyword "~"
  argumentsAndReturnMangement <- parseArgumentsManagement
  let
    argumentsManagementList = List.map (first fromJust) $ List.filter (\(a, _) -> isJust a) $ List.map
      (\(a, d) -> (M.lookup (MkValue a) argsMap, d))
      argumentsAndReturnMangement
  let
    returnManagement =
      case
          List.filter
            (\(a, _) -> case fsReturn functionSignature of
              NoReturn -> False
              Return r -> r == a
            )
            argumentsAndReturnMangement
        of
          []           -> Nothing
          ((a, d) : _) -> Just d
  let argumentsManagement = M.fromList argumentsManagementList
  return MkCallMemoryManagementDirectives { RMD.functionSignature, RMD.argumentsManagement, RMD.returnManagement }

parseArgumentsManagement :: Parser [(Text, MemoryManagementDirective)]
parseArgumentsManagement = do
  argManagementHead     <- parseArgumentManagement
  argManagementHeadTail <- many $ do
    void $ lexemeSc $ char ','
    parseArgumentManagement
  let result = argManagementHead : argManagementHeadTail
  return result

parseArgumentManagement :: Parser (Text, MemoryManagementDirective)
parseArgumentManagement = do
  arg <- lexemeSc identifier
  keyword "="
  directive <- try parseAllocHere <|> try parseAllocAbove
  return (arg, directive)

parseAllocHere = do
  keyword "here"
  return AllocHere
parseAllocAbove = do
  keyword "up"
  return AllocAbove

