module ValueSizeWriters
  ( writeValueSizeAnalyticsMap
  ) where

import Data.Text as T (Text, intercalate, concat, pack, unwords)
import ValueSizeData
  ( ValueSizeAnalyticsValues(MkValueSizeAnalyticsValues, argumentSizes, resultSize, callArgSizes, internalSizes)
  , ValueSizeAnalytics
  , ValueSizeAnalyticsValue(Bits, Pointer)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithArgSizes(MkFunctionCallWithSizes, argSizes, functionCall, retSize)
  )
import Data.Map as M (toList, fromList, Map, lookup, lookup)
import Data.Set as S (toList)
import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs))
import AspectWriters (writeFunctionSignature)
import Data.List.Index (indexed)
import Prelude hiding (concat)
import Debug.Trace (trace)
import Data.List (sortBy)
import Data.Maybe (isJust)
import SemanticTree (SemanticTree, Statement, FunctionSignature(fsArgs), unpackArg)
import Location (Ranged)
import Shared (justGet, justLookup, showT)
import AspectsShared as AS (writeStatement, extractFunctions)
import SemanticTree as Sem (FunctionSignature)
import Data.List as L (length)
import SemanticShared as SH (writeStatement)

writeValueSizeAnalyticsMap :: SemanticTree -> ValueSizeAnalytics -> Text
writeValueSizeAnalyticsMap tree =
  intercalate "\n"
    . map (writeValueSizeAnalyticsValue (extractFunctions tree) tree)
    . sortBy (\(l, _) (r, _) -> compare l r)
    . concatMap (\(s, values) -> foldl (\acc v -> (s, v) : acc) [] values)
    . M.toList

writeValueSizeAnalyticsValue
  :: M.Map Sem.FunctionSignature [Ranged Statement]
  -> SemanticTree
  -> (FunctionSignature, ValueSizeAnalyticsValues)
  -> Text
writeValueSizeAnalyticsValue functions tree (sig, v) =
  let
    function = justLookup sig functions
    args     = map unpackArg $fsArgs sig
  in T.concat [writeFunctionSignature sig, " ~ ", writeValueSizeValues (sig, function) tree args v]

writeValueSizeValues
  :: (FunctionSignature, [Ranged Statement]) -> SemanticTree -> [Text] -> ValueSizeAnalyticsValues -> Text
writeValueSizeValues function tree args MkValueSizeAnalyticsValues { argumentSizes = argSizes, resultSize = result, callArgSizes = callArgSizes }
  = let
      argStrings =
        map
            (\(i, a) -> case M.lookup i argSizes of
              Nothing -> ""
              Just c  -> concat [a, "= ", writeValueSizeValue c]
            )
          $ indexed args
      resultStrings = result
      signatureStr  = intercalate ", " $ case result of
        Nothing  -> argStrings
        Just res -> argStrings ++ [writeResultValueSizeValues res]
    in if null callArgSizes
      then signatureStr
      else intercalate
        "\n"
        (signatureStr : map (writePositionedFunctionCall function) (sortPositionedFunctionCalls (S.toList callArgSizes))
        )

sortPositionedFunctionCalls :: [PositionedFunctionCall] -> [PositionedFunctionCall]
sortPositionedFunctionCalls =
  sortBy (\MkPositionedFunctionCall { callPos = posA } MkPositionedFunctionCall { callPos = posB } -> compare posA posB)


writePositionedFunctionCall :: (FunctionSignature, [Ranged Statement]) -> PositionedFunctionCall -> Text
writePositionedFunctionCall (sig, statements) MkPositionedFunctionCall { callPos = callPos, functionCalls = functionCalls }
  = let
      pos          = showT callPos
      commentedOut = L.length functionCalls /= 1
    in concat
      [ if commentedOut then "|" else ""
      , "\t"
      , pos
      , " "
      , case functionCalls of
        [                  singleFuncCall] -> writePositionedFunctionCallWithSizes singleFuncCall
        multipleFuncCalls@(_ : _         ) -> concat
          [ if commentedOut
            then SH.writeStatement (sig, let pos = fromIntegral (callPos - 1) in justGet statements pos)
            else AS.writeStatement (sig, let pos = fromIntegral (callPos - 1) in justGet statements pos)
          , concat $ map (\fc -> concat ["\n\t", pos, " ", writePositionedFunctionCallWithSizes fc]) $ reverse
            functionCalls
          ]
        [] -> ""
      ]

writePositionedFunctionCallWithSizes :: FunctionCallWithArgSizes -> Text
writePositionedFunctionCallWithSizes MkFunctionCallWithSizes { argSizes = argSizes, retSize = retSize, functionCall = MkFunctionCall { fcName = name, fcArgs = args } }
  = concat
    $  [ name
       , " "
       , T.unwords args
       , " ~ "
       , intercalate ", " $ zipWith (\a v -> concat [a, "= ", writeValueSizeValue v]) args argSizes
       ]
    ++ (case retSize of
         Nothing -> []
         Just v  -> " -> " : [writeValueSizeValue v]
       )

writeResultValueSizeValues result = concat ["r= ", writeValueSizeValue result]

writeValueSizeValue :: ValueSizeAnalyticsValue -> Text
writeValueSizeValue Pointer    = "pointer"
writeValueSizeValue (Bits 1  ) = "1 bit"
writeValueSizeValue (Bits cnt) = concat [showT cnt, " bits"]
