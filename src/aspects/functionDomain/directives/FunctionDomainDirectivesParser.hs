module FunctionDomainDirectivesParser
  ( functionDomainDirectivesParser
  , defaultConstraint
  ) where

import Prelude (Integer)
import Control.Monad (fail, void, when, Monad(return), mapM_)
import Data.Map as M (Map, fromList, lookup)
import Data.Set as S (difference, fromList, toList)
import Data.List (groupBy, intercalate, length, filter, map, (++), head, null, concat)
import Data.Maybe (fromJust, fromMaybe, maybeToList, Maybe(Nothing), Maybe(Just))
import Data.Text (Text, unpack)
import Data.Void (Void)
import Debug.Trace (trace)
import Data.Tuple (swap, snd, fst)
import Text.Megaparsec (Parsec, (<|>), eof, getSourcePos, many, optional, some, try)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Stream ()
import Data.List.Index (indexed)

import AspectsParser (parseSemanticFunction)
import FunctionDomainDirectivesData as FD
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , from
  , input
  , output
  , to
  , FunctionDomainConstraintFrom(MkIntegerFrom, MkNegInfinite)
  , FunctionDomainConstraintTo(MkIntegerTo, MkPosInfinite)
  )
import Location as L (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import SyntaxParser (Parser, assign, equals, identifier, integer, lexemeSc, parseSkipNewLine, isAbout)
import SyntaxToSemantic (stsSignature)
import Data.String (String)
import Control.Applicative (Applicative((<*)), (<$>))
import Data.Function (($), (.))
import Data.Eq (Eq((/=)), (==))
import Data.Ord (Ord((>)))
import Shared (justLookup)
import SemanticTree
  (FunctionSignature(fsArgs, fsReturn), Return(Return, NoReturn), unpackArg, Arg(MkValue), unpackReturn)
import Data.Bifunctor (Bifunctor(first))
import Text.Show (Show(show))
import Data.Bool (Bool(False, True))

functionDomainDirectivesParser :: Parser [Ranged (FunctionSignature, FunctionDomainConstraint)]
functionDomainDirectivesParser = many functionDomainDirective <* eof

functionDomainDirective :: Parser (Ranged (FunctionSignature, FunctionDomainConstraint))
functionDomainDirective = parseFunctionDomainItem

functionDomainAnalyticsItem :: Parser (Ranged (FunctionSignature, FunctionDomainConstraint))
functionDomainAnalyticsItem = parseFunctionDomainItem

parseFunctionDomainItem :: Parser (Ranged (FunctionSignature, FunctionDomainConstraint))
parseFunctionDomainItem = do
  from <- getSourcePos
  sig  <- parseSemanticFunction
  isAbout
  val <- parseItemValue sig
  to  <- getSourcePos
  parseSkipNewLine
  return MkRanged { rItem = (sig, val), range = MkRange { L.from, L.to } }

parseItemValue :: FunctionSignature -> Parser FunctionDomainConstraint
parseItemValue sig = do
  maybeInput <- optional $ parseInConstrainsByArgs sig
  let args        = M.fromList $ map swap $indexed $ fsArgs sig
  let constraints = fromMaybe [] maybeInput
  let
    inputConstraints = map (first MkValue) $ filter
      (\(n, _) -> case fsReturn sig of
        NoReturn -> True
        Return r -> n /= r
      )
      constraints
  let
    inputConstraintsList = -- trace("\ninputConstraints: "++show inputConstraints) $ 
      map (\(name, c) -> (justLookup name args, c)) inputConstraints
  let
    input = -- trace ("\ninputConstraintsList: "++show inputConstraintsList) $
      M.fromList inputConstraintsList
  let
    output =
      case
          filter
            (\(n, _) -> case fsReturn sig of
              NoReturn -> False
              Return r -> n == r
            )
            constraints
        of
          []      -> Nothing
          (v : _) -> Just (snd v)
  return MkFunctionDomainConstraint { input, output }

parseInConstrainsByArgs :: FunctionSignature -> Parser [(Text, [FunctionDomainInOutConstraints])]
parseInConstrainsByArgs sig = do
  headConstraints <- parseConstraintsByOr sig
  constraintsTail <- many $ do
    void $ lexemeSc $ char ','
    parseConstraintsByOr sig
  let constraints = headConstraints : constraintsTail
  parseValidateUndefinedAliases sig constraints
  parseGroupConstraintsByArg constraints


lookupConstraint name constraints = fromMaybe [defaultConstraint] (lookup name constraints)

defaultConstraint :: FunctionDomainInOutConstraints
defaultConstraint = MkRangedFunctionDomainInOutConstraint { FD.from = MkNegInfinite, FD.to = MkPosInfinite }

extractAliases :: FunctionSignature -> [Text]
extractAliases sig =
  let
    args = map unpackArg $fsArgs sig
    ret  = unpackReturn (fsReturn sig)
  in case ret of
    Nothing -> args
    Just r  -> r : args

parseGroupConstraintsByArg
  :: [[(Text, FunctionDomainInOutConstraints)]] -> Parser [(Text, [FunctionDomainInOutConstraints])]
parseGroupConstraintsByArg constraints = do
  let groupedConstraints = map (groupBy (\(tl, _) (tr, _) -> tl == tr)) constraints
  parseValidateNoMoreAliasPerGroup groupedConstraints
  return $ map (\subList -> (fst $ head subList, map snd subList)) $ concat groupedConstraints

parseValidateNoMoreAliasPerGroup :: [[[(Text, FunctionDomainInOutConstraints)]]] -> Parser ()
parseValidateNoMoreAliasPerGroup = mapM_
  (\grouped -> when (length grouped > 1)
    $ fail ("There are more than 1 alias inside constraints group: " ++ extractAlieasesString grouped)
  )

extractAlieasesString :: [[(Text, FunctionDomainInOutConstraints)]] -> String
extractAlieasesString grouped = intercalate ", " $ map (unpack . fst) (concat grouped)

parseValidateUndefinedAliases :: FunctionSignature -> [[(Text, FunctionDomainInOutConstraints)]] -> Parser ()
parseValidateUndefinedAliases sig constraints =
  let
    aliases            = S.fromList $ extractAliases sig
    constraintNames    = S.fromList $ map fst $ concat constraints
    unknownConstraints = S.difference constraintNames aliases
  in if null unknownConstraints
    then return ()
    else fail ("There is no alias '" ++ unpack (head $ S.toList unknownConstraints) ++ "'")

parseConstraintsByOr :: FunctionSignature -> Parser [(Text, FunctionDomainInOutConstraints)]
parseConstraintsByOr sig = do
  headConstraint <- parseInOutConstraint
  constraints    <- many $ do
    void $ lexemeSc $ string "or"
    parseInOutConstraint
  return $ headConstraint : constraints

parseInOutConstraint :: Parser (Text, FunctionDomainInOutConstraints)
parseInOutConstraint = do
  from <- parseFromConstraint
  name <- lexemeSc identifier
  to   <- parseToConstraint
  return (name, MkRangedFunctionDomainInOutConstraint { FD.from, FD.to })

parseFromConstraint :: Parser FunctionDomainConstraintFrom
parseFromConstraint = do
  value <- lexemeSc parseFromValue
  parseLessOrEqual
  return value

parseToConstraint :: Parser FunctionDomainConstraintTo
parseToConstraint = do
  lexemeSc parseLessOrEqual
  lexemeSc parseToValue

parseToValue = MkIntegerTo <$> integer <|> parsePosInf

parseLessOrEqual = void $ lexemeSc $ string "<="

parseFromValue = (MkIntegerFrom <$> try integer) <|> try parseNegInf

parseNegInf = do
  void $ lexemeSc $ char '-'
  void $ lexemeSc $ string "inf"
  return MkNegInfinite

parsePosInf = do
  void $ lexemeSc $ string "inf"
  return MkPosInfinite
