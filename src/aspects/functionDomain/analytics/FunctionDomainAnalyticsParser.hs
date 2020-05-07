module FunctionDomainAnalyticsParser
  ( functionDomainAnalyticsParser
  ) where

import Prelude ()
import Text.Megaparsec.Debug (dbg)
import Control.Monad (fail, void, when, Monad(return))
import Data.Map as M (Map, fromList, lookup)
import Data.Set as S (difference, fromList, toList, insert, empty, union, Set, singleton)
import Data.List (groupBy, intercalate, length, map, head, tail, (++), foldl, foldl1, reverse)
import Data.Maybe (fromJust, fromMaybe, maybeToList, Maybe(Nothing, Just), isJust)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Control.Applicative (Applicative((<*)))
import Data.Function (($), (.))
import Debug.Trace (trace)
import Text.Megaparsec
  (Parsec, (<|>), eof, getSourcePos, many, optional, some, try, failure, ErrorItem(EndOfInput), customFailure)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Stream ()
import Text.Megaparsec.Char.Lexer (IndentOpt(IndentSome, IndentMany), decimal, indentBlock, lexeme, nonIndented, space)

import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import AspectsParser (parseSemanticFunction, parseSkipNewLine, parseInt, parseFuncCall)
import FunctionDomainAnalyticsData
  ( UndefinedFunctionProbability(MkUndefinedFunctionProbability, callUndefinedProbabilities, resultUndefinedProbability)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, callProbability, functionCall)
  , UndefinedProbability(Defined, MaybeDefined)
  , CallProbability(WillBeCalled, MaybeWillBeCalled)
  )
import Location as L (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import SyntaxParser (Parser, assign, equals, identifier, integer, lexemeSc, keyword, isAbout)
import Data.Eq (Eq((==)))
import Text.Show (Show(show))
import SemanticTree (FunctionSignature(fsArgs, fsReturn), unpackArg, unpackReturn)
import Data.Ord (Ord)

functionDomainAnalyticsParser :: Parser [Ranged (FunctionSignature, UndefinedFunctionProbability)]
functionDomainAnalyticsParser = many undefinedProbabilityItem <* eof

groupByCallPos ungroupedCallUndefinedProbabilities =
  let
    groupedCallUndefinedProbabilities =
      groupBy (\left right -> callPos left == callPos right) ungroupedCallUndefinedProbabilities
    callUndefinedProbabilities = map
      (\a -> MkPositionedFunctionCall
        { callPos       = callPos $ head a
        , functionCalls = foldl (\acc curr -> S.union (functionCalls curr) acc) S.empty a
        }
      )
      groupedCallUndefinedProbabilities
  in S.fromList callUndefinedProbabilities

undefinedProbabilityItem :: Parser (Ranged (FunctionSignature, UndefinedFunctionProbability))
undefinedProbabilityItem = do
  from <- getSourcePos
  (sig, resultUndefinedProbability, ungroupedCallUndefinedProbabilities) <-
    nonIndented parseSkipNewLine $ indentBlock parseSkipNewLine $ do
      sig <- parseSemanticFunction
      isAbout
      resProb <- case unpackReturn $ fsReturn sig of
        Nothing   -> return Nothing
        Just name -> optional $parseResultUndefinedProbability name
      return $ IndentMany Nothing (return . (sig, resProb, )) $ parseItemValue $ map unpackArg $ fsArgs sig
  to <- getSourcePos
  let callUndefinedProbabilities = groupByCallPos ungroupedCallUndefinedProbabilities
  let val = MkUndefinedFunctionProbability { callUndefinedProbabilities, resultUndefinedProbability }
  return $ --trace ("\nVAL: "++show val) $
           MkRanged { rItem = (sig, val), range = MkRange { L.from, L.to } }

parseResultUndefinedProbability :: Text -> Parser UndefinedProbability
parseResultUndefinedProbability retName = do
  keyword retName
  keyword "="
  parseUndefinedProbability

parseItemValue :: [Text] -> Parser PositionedFunctionCall
parseItemValue args = do
  callPos      <- --dbg "callPos" $ 
                  lexemeSc parseInt
  functionCall <- --dbg "functionCalls" $ 
                  parseFuncCallWithProbability
  return $ MkPositionedFunctionCall { callPos, functionCalls = S.singleton functionCall }

parseFuncCallWithProbability :: Parser FunctionCallWithProbability
parseFuncCallWithProbability = do
  functionCall <- --dbg "parseFuncCall" $ 
                  parseFuncCall
  keyword "~"
  callProbability <- --dbg "parseUndefinedProbability" $ 
                     parseCallProbability
  when (isJust $ tmpVarName functionCall) $ do
    keyword ";"
    lexemeSc identifier
    keyword "="
    if callProbability == MaybeWillBeCalled then parseMaybeDefined else parseDefined
    return ()

  return MkFunctionCallWithProbability { callProbability, functionCall }

parseCallProbability :: Parser CallProbability
parseCallProbability = parseWillBeCalled <|> parseMaybeWillBeCalled

parseWillBeCalled = do
  parseVoidWillBeCalled
  return WillBeCalled

parseMaybeWillBeCalled = do
  parseVoidMaybeWillBeCalled
  return MaybeWillBeCalled

parseUndefinedProbability :: Parser UndefinedProbability
parseUndefinedProbability = parseDefined <|> parseMaybeDefined

parseDefined = do
  parseVoidDefined
  return Defined

parseMaybeDefined = do
  parseVoidMaybe
  parseVoidDefined
  return MaybeDefined

parseVoidDefined = keyword "defined"
parseVoidMaybe = keyword "maybe"

parseVoidWillBeCalled = do
  keyword "will"
  keyword "be"
  keyword "called"

parseVoidMaybeWillBeCalled = do
  keyword "maybe"
  keyword "will"
  keyword "be"
  keyword "called"
