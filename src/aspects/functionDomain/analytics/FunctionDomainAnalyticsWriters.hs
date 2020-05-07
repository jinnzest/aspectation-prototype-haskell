module FunctionDomainAnalyticsWriters
  ( writeFunctionDomainAnalyticsMap
  ) where

import Prelude (Integer, Num((-)), fromIntegral)
import Data.Maybe (maybe, Maybe(Just), Maybe, Maybe(Nothing), fromMaybe)
import Data.List as L (null, length, map, head, foldl, (!!), (++), filter, any, reverse, sort)
import Data.Sort (sortBy)
import Data.Text as T (Text, append, concat, empty, intercalate, length, null, pack, unpack)
import Debug.Trace (trace)

import AspectWriters (writeFunctionSignature, writeFunctionCall)
import FunctionDomainAnalyticsData
  ( FunctionDomainAnalytics
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, callProbability, functionCall)
  , UndefinedFunctionProbability(MkUndefinedFunctionProbability, callUndefinedProbabilities, resultUndefinedProbability)
  , UndefinedProbability(Defined, MaybeDefined)
  , UndefinedProbabilityAnalytics
  , CallProbability(WillBeCalled, MaybeWillBeCalled)
  )
import FunctionDomainDirectivesData
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , from
  , input
  , output
  , to
  )
import Shared (justLookup, justGet, showT)
import Data.Tuple (uncurry, fst)
import Data.Function (($), (.))
import Text.Show (Show(show))
import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import Data.Eq (Eq((==)))
import SemanticTree
  ( SemanticTree(_semanticTree)
  , Function(MkFunction, fSignature, fBody)
  , Statement(MkNoAssignment)
  , Arg(MkValue)
  , Return(NoReturn, Return)
  , Expression(MkConstantInteger, MkFunctionArgument, fcsName, fcsArgs)
  , FunctionSignature
  , fsReturn
  )
import Data.Map as M (Map, lookup, empty, member, insert, toList, fromList)
import Location (Ranged(MkRanged, rItem, range), Positioned(pos))
import Maybes (fromJust)
import AspectsData as Asp ()
import SemanticTree as Sem (FunctionSignature)
import Data.Ord ((>), Ord((<=), (>=), compare))
import Panic (panic)
import Data.Bool (Bool(False, True), not)
import AspectsShared (writeStatement, extractFunctions)
import Data.Set as S (toList, size, Set, map, union, foldl)
import Location as L (Range(from))
import Text.Megaparsec (SourcePos(SourcePos))
import Text.Megaparsec.Pos (unPos)
import Data.Int (Int)
import Data.List.Index (indexed)
import BasicPrelude (Num((+)))
import SemanticShared as SH (writeStatement)


writeFunctionDomainAnalyticsMap :: SemanticTree -> UndefinedProbabilityAnalytics -> Text
writeFunctionDomainAnalyticsMap tree analytics = -- trace ("\n\nwriteFunctionDomainAnalyticsMap:\ntree: "++show tree++"\nanalytics: "++show analytics)
  intercalate "\n" $ sort $ L.map (writeAnalyticsItem (extractFunctions tree)) $ M.toList analytics

writeAnalyticsItem
  :: M.Map Sem.FunctionSignature [Ranged Statement] -> (FunctionSignature, UndefinedFunctionProbability) -> Text
writeAnalyticsItem functions (sig, probabilities) =
  let statements = fromMaybe [] (lookup sig functions)
  in concat [writeFunctionSignature sig, " ~ ", writeUndefinedProbabilities (sig, statements) probabilities, "\n"]

writeUndefinedProbabilities :: (FunctionSignature, [Ranged Statement]) -> UndefinedFunctionProbability -> Text
writeUndefinedProbabilities function@(sig, statements) MkUndefinedFunctionProbability { callUndefinedProbabilities = callUndefinedProbabilities, resultUndefinedProbability = resultUndefinedProbability }
  = let resultProbability = writeResultFunctionDomain (fsReturn $ fst function) resultUndefinedProbability
    in
      intercalate
        "\n"
        (resultProbability : L.map
          (writePositionedFunctionCall sig callUndefinedProbabilities . (\(p, s) -> (p + 1, s)))
          (indexed statements)
        )

writeResultFunctionDomain :: Return -> Maybe UndefinedProbability -> Text
writeResultFunctionDomain (Return name) (Just res) = concat [name, "= ", writeProbability res]
writeResultFunctionDomain NoReturn Nothing = ""
writeResultFunctionDomain r p = panic ("\nunexpected combination, name=" ++ show r ++ ", probability=" ++ show p)

writePositionedFunctionCall :: FunctionSignature -> Set PositionedFunctionCall -> (Int, Ranged Statement) -> Text
writePositionedFunctionCall sig positionedFuncCalls (pos, rStatement@MkRanged { rItem = statement, range = range }) =
  let
    positionedFuncCallsMap =
      -- trace ("\n\nwritePositionedFunctionCall: " ++ show sig ++ "\npositionedFuncCalls: " ++ show positionedFuncCalls) $
      toFunctionCallsWithProbability positionedFuncCalls
  in
    case M.lookup pos positionedFuncCallsMap of
      Nothing            -> concat ["|\t", showT pos, " ", SH.writeStatement (sig, rStatement)]
      Just functionCalls -> case S.size functionCalls of
        0 -> panic $ "empty function calls at pos " ++ show pos
        1 -> concat ["\t", showT pos, " ", writeFunctionCallWithProbability $ head $ S.toList functionCalls]
        _ -> concat
          [ "|\t"
          , showT pos
          , " "
          , SH.writeStatement (sig, rStatement)
          , concat $ L.map (\fc -> concat ["\n\t", showT pos, " ", writeFunctionCallWithProbability fc]) $ S.toList
            functionCalls
          ]

toFunctionCallsWithProbability :: Set PositionedFunctionCall -> Map Int (Set FunctionCallWithProbability)
toFunctionCallsWithProbability = S.foldl
  (\acc t ->
    let
      pos   = callPos t
      calls = functionCalls t
    in M.insert
      pos
      (case M.lookup pos acc of
        Nothing -> calls
        Just fc -> calls `S.union` fc
      )
      acc
  )
  M.empty

writeFunctionCallWithProbability :: FunctionCallWithProbability -> Text
writeFunctionCallWithProbability MkFunctionCallWithProbability { callProbability = callProbability, functionCall = functionCall@MkFunctionCall { tmpVarName = tvn } }
  = let
      resultProb = case tvn of
        Nothing -> []
        Just n  -> [";", n, "=", if callProbability == WillBeCalled then "defined" else "maybe defined"]
    in intercalate " " $ [writeFunctionCall functionCall, "~", writeCallProbability callProbability] ++ resultProb

writeProbability :: UndefinedProbability -> Text
writeProbability Defined      = "defined"
writeProbability MaybeDefined = "maybe defined"

writeCallProbability :: CallProbability -> Text
writeCallProbability WillBeCalled      = "will be called"
writeCallProbability MaybeWillBeCalled = "maybe will be called"
