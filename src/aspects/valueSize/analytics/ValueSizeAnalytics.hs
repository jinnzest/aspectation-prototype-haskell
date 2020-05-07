module ValueSizeAnalytics
  ( generateValueSizeAnalytics
  , embeddedValueSizeAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO, Except)
import Data.Map as M
  ( Map
  , empty
  , toList
  , lookup
  , fromList
  , singleton
  , union
  , insert
  , insertWith
  , member
  , Map
  , singleton
  , union
  , fromList
  , lookup
  )
import Data.Set as S (Set, empty, fromList, toList)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, ($), return, Int, Bounded(minBound, maxBound), Integral(toInteger), Integer, (+))
import Shared (justLookup)
import Errors (Errors)
import SemanticTree as Sem
  ( SemanticTree(MkSemanticTree, _semanticTree)
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Function(fSignature, MkFunction, fBody)
  , Statement(MkNoAssignment, expression)
  , Expression(fcsName, fcsArgs, tmpVarName, MkFunctionCall, MkFunctionArgument, MkConstantInteger)
  , Arg(MkValue)
  , Return(NoReturn, Return)
  , tmpVarToReturn
  )
import FunctionDomainAnalyticsData (FunctionDomainAnalytics)
import ValueSizeData
  ( ValueSizeAnalyticsValues(MkValueSizeAnalyticsValues, resultSize, argumentSizes, callArgSizes, internalSizes)
  , ValueSizeAnalytics
  , ValueSizeAnalyticsValue(Pointer, Bits)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithArgSizes(MkFunctionCallWithSizes, argSizes, retSize)
  , functionCall
  , ArgumentSizes
  )
import Data.List (map, concat, concatMap, (++), maximum, filter, foldl, last, (!!), head)
import Data.Tuple (snd, fst)
import Data.Maybe (fromJust, Maybe(Nothing, Just), maybe, isJust)
import FunctionDomainDirectivesData
  ( FunctionDomainDirectives
  , FunctionDomainConstraint(MkFunctionDomainConstraint, input, output)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint, from, to)
  , FunctionDomainConstraintTo(MkIntegerTo, MkPosInfinite)
  , FunctionDomainConstraintFrom(MkIntegerFrom, MkNegInfinite)
  )
import Data.Function ((.), flip, const)
import Data.Int (Int64, Int32, Int16, Int8)
import Data.Ord (Ord((<), (>), (>=), (<=), max))
import Data.Bool ((||), otherwise, Bool(True, False))
import Debug.Trace (trace)
import Text.Show (Show(show))
import Data.Eq (Eq((==)))
import Data.List.Index (indexed)
import Location (Ranged(MkRanged, range, rItem))
import Data.Text (Text)
import AspectsData as Asp (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import Panic (panic)
import GHC.Generics (Generic)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Control.Monad.State.Lazy (State, mapM_, runState, MonadState(get), modify, mapM)
import Control.Applicative ((<$>))
import MonadUtils (concatMapM)

data Context = MkContext
  { functions          :: Map Sem.FunctionSignature [Ranged Statement]
  , functionSignatures :: Map Text Sem.FunctionSignature
  , analytics          :: ValueSizeAnalytics
  , callers            :: Set Sem.FunctionSignature
  , directives         :: FunctionDomainDirectives
  }
  deriving Generic

instance Show Context where
  show _ = ""

embeddedValueSizeAnalytics :: ValueSizeAnalytics
embeddedValueSizeAnalytics =
  M.singleton MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn } $ M.fromList
    [ ( M.singleton 0 Pointer
      , MkValueSizeAnalyticsValues
        { argumentSizes = M.singleton 0 Pointer
        , internalSizes = M.empty
        , resultSize    = Nothing
        , callArgSizes  = S.empty
        }
      )
    , ( M.singleton 0 (Bits 64)
      , MkValueSizeAnalyticsValues
        { argumentSizes = M.singleton 0 (Bits 64)
        , internalSizes = M.empty
        , resultSize    = Nothing
        , callArgSizes  = S.empty
        }
      )
    , ( M.singleton 0 (Bits 32)
      , MkValueSizeAnalyticsValues
        { argumentSizes = M.singleton 0 (Bits 32)
        , internalSizes = M.empty
        , resultSize    = Nothing
        , callArgSizes  = S.empty
        }
      )
    , ( M.singleton 0 (Bits 16)
      , MkValueSizeAnalyticsValues
        { argumentSizes = M.singleton 0 (Bits 16)
        , internalSizes = M.empty
        , resultSize    = Nothing
        , callArgSizes  = S.empty
        }
      )
    , ( M.singleton 0 (Bits 8)
      , MkValueSizeAnalyticsValues
        { argumentSizes = M.singleton 0 (Bits 8)
        , internalSizes = M.empty
        , resultSize    = Nothing
        , callArgSizes  = S.empty
        }
      )
    , ( M.singleton 0 (Bits 1)
      , MkValueSizeAnalyticsValues
        { argumentSizes = M.singleton 0 (Bits 1)
        , internalSizes = M.empty
        , resultSize    = Nothing
        , callArgSizes  = S.empty
        }
      )
    ]

generateValueSizeAnalytics
  :: [Sem.FunctionSignature] -> SemanticTree -> FunctionDomainDirectives -> ValueSizeAnalytics -> ValueSizeAnalytics
generateValueSizeAnalytics embeddedFuncs MkSemanticTree { _semanticTree = semanticTree } directives analytics =
  let
    functions          = M.fromList $ map (\f -> (fSignature f, fBody f)) $concatMap snd (M.toList semanticTree)
    functionSignatures = foldl
      (\acc sig@Sem.MkFunctionSignature { Sem.fsName = name } -> insert name sig acc)
      M.empty
      (embeddedFuncs ++ map fSignature (concatMap snd $ M.toList semanticTree))
    callers = S.empty
  in fst $ runState
    generateValueSizeAnalyticsWithContext
    MkContext { functions, functionSignatures, analytics, callers, directives }

generateValueSizeAnalyticsWithContext :: State Context ValueSizeAnalytics
generateValueSizeAnalyticsWithContext = do
  context <- get
  let functionSignaturesAndBodies = M.toList $ functions context
  mapM_ generateSignatureValueSizeAnalytics functionSignaturesAndBodies
  analytics <$> get

generateSignatureValueSizeAnalytics :: (Sem.FunctionSignature, [Ranged Statement]) -> State Context ()
generateSignatureValueSizeAnalytics (signature@Sem.MkFunctionSignature { Sem.fsName = fsName, Sem.fsArgs = args }, statements)
  = do
    context <- get
    let directivesContext = M.lookup signature $ directives context
    case directivesContext of
      Nothing  -> return ()
      Just ctx -> if M.member signature $ analytics context
        then return ()
        else do
          callArgSizesList <-
            mapM
              (\(i, MkRanged { rItem = MkNoAssignment { expression = e } }) -> do
                functionCalls <- generateFunctionCallValueSizeAnalytics (input ctx) e
                return MkPositionedFunctionCall { callPos = toInteger i + 1, functionCalls }
              )
            $ filter filterFunctionCall
            $ indexed statements
          let callArgSizes  = S.fromList callArgSizesList
          let argumentSizes = generateArgsValueSizeAnalytics ctx
          resultSz <- returnValueSizeAnalytics argumentSizes $ last statements
          let
            valueSizeAnalytics = M.singleton argumentSizes $ MkValueSizeAnalyticsValues
              { argumentSizes
              , internalSizes = M.empty
              , resultSize    = case output ctx of
                Nothing -> Nothing
                Just v  -> Just resultSz
              , callArgSizes
              }
          modify
            (\newContext ->
              let
                newCtx = MkContext
                  { functions          = functions newContext
                  , directives         = directives newContext
                  , callers            = callers newContext
                  , analytics          =
                    let f new old = M.union new old
                    in M.insertWith f signature valueSizeAnalytics $analytics newContext
                  , functionSignatures = functionSignatures newContext
                  }
              in newCtx
            )

generateArgsValueSizeAnalytics :: FunctionDomainConstraint -> ArgumentSizes
generateArgsValueSizeAnalytics =
  M.fromList . map (\(i, v) -> (i, selectBiggestValueSize $ map generateValueSize v)) . M.toList . input

returnValueSizeAnalytics :: ArgumentSizes -> Ranged Statement -> State Context ValueSizeAnalyticsValue
returnValueSizeAnalytics _ MkRanged { rItem = MkNoAssignment { expression = MkConstantInteger _ MkRanged { rItem = i } } }
  = return $ generateConstantValueSize i
returnValueSizeAnalytics args MkRanged { rItem = MkNoAssignment { expression = MkFunctionArgument _ MkRanged { rItem = i } } }
  = return $ fromJust $ M.lookup i args
returnValueSizeAnalytics _ MkRanged { rItem = MkNoAssignment { expression = Sem.MkFunctionCall { fcsName = MkRanged { rItem = fcsName }, fcsArgs = fcsArgs, Sem.tmpVarName = tvn } } }
  = do
    ctx <- get
    let ret = tmpVarToReturn tvn
    let
      signature = Sem.MkFunctionSignature
        { Sem.fsName   = fcsName
        , Sem.fsArgs   = map (const $ MkValue "v") fcsArgs
        , Sem.fsReturn = ret
        }
    case M.lookup signature $ functions ctx of
      Just function -> generateSignatureValueSizeAnalytics (signature, function)
      Nothing       -> return ()
    ctx <- get
    let result = fromJust $ resultSize $ snd $ head $ M.toList $ justLookup signature $ analytics ctx
    return result

generateFunctionCallValueSizeAnalytics
  :: Map Int [FunctionDomainInOutConstraints] -> Expression -> State Context [FunctionCallWithArgSizes]
generateFunctionCallValueSizeAnalytics argsMap (MkFunctionArgument _ i) = return []
generateFunctionCallValueSizeAnalytics argsMap (MkConstantInteger  _ i) = return []
generateFunctionCallValueSizeAnalytics argsMap func@Sem.MkFunctionCall { fcsName = MkRanged { rItem = fcName }, fcsArgs = args, Sem.tmpVarName = tvn }
  = do
    context <- get
    let signatures = functionSignatures context
    let funcArgs   = Sem.fsArgs $ justLookup fcName signatures
    let ret        = tmpVarToReturn tvn
    argSizes     <- concatMapM (generateExpressionValueSizeAnalytics argsMap) args
    argFuncCalls <- concatMapM (generateFunctionCallValueSizeAnalytics argsMap) args
    retSize      <- generateReturnSizeAnalytics fcName args ret
    return
      $ MkFunctionCallWithSizes
          { functionCall = Asp.MkFunctionCall
            { fcName
            , fcArgs         = map (\(MkValue a) -> a) funcArgs
            , Asp.tmpVarName = tvn
            }
          , argSizes
          , retSize
          }
      : argFuncCalls

generateReturnSizeAnalytics :: Text -> [Expression] -> Return -> State Context (Maybe ValueSizeAnalyticsValue)
generateReturnSizeAnalytics name args ret = do
  context <- get
  let signature = justLookup name $functionSignatures context
  case M.lookup signature $ functions context of
    Nothing   -> return ()
    Just body -> generateSignatureValueSizeAnalytics (signature, body)
  context <- get
  let functionAnalytics = justLookup signature $ analytics context
  case ret of
    NoReturn -> return Nothing
    Return _ -> do
      let result = map fromJust $filter isJust $ map (resultSize . snd) (M.toList functionAnalytics)
      return $ Just $ selectBiggestValueSize result

generateExpressionValueSizeAnalytics
  :: Map Int [FunctionDomainInOutConstraints] -> Expression -> State Context [ValueSizeAnalyticsValue]
generateExpressionValueSizeAnalytics argsMap (MkFunctionArgument _ MkRanged { rItem = i }) =
  return $ map generateValueSize $ justLookup i argsMap
generateExpressionValueSizeAnalytics argsMap (MkConstantInteger _ MkRanged { rItem = i, range = range }) =
  return [generateValueSize MkRangedFunctionDomainInOutConstraint { from = MkIntegerFrom i, to = MkIntegerTo i }]
generateExpressionValueSizeAnalytics argsMap Sem.MkFunctionCall { fcsName = MkRanged { rItem = fcName }, fcsArgs = args }
  = do
    context <- get
    let signature = justLookup fcName $functionSignatures context
    let body      = justLookup signature $ functions context
    generateSignatureValueSizeAnalytics (signature, body)
    context <- get
    let functionAnalytics = justLookup signature $ analytics context
    let result = map fromJust $filter isJust $ map (resultSize . snd) (M.toList functionAnalytics)
    return result

generateConstantValueSize :: Integer -> ValueSizeAnalyticsValue
generateConstantValueSize i =
  generateValueSize MkRangedFunctionDomainInOutConstraint { from = MkIntegerFrom i, to = MkIntegerTo i }

filterFunctionCall :: (a, Ranged Statement) -> Bool
filterFunctionCall (_, MkRanged { rItem = MkNoAssignment { expression = Sem.MkFunctionCall{} } }) = True
filterFunctionCall (_, _) = False

selectBiggestValueSize :: [ValueSizeAnalyticsValue] -> ValueSizeAnalyticsValue
selectBiggestValueSize = maximum

generateValueSize :: FunctionDomainInOutConstraints -> ValueSizeAnalyticsValue
generateValueSize MkRangedFunctionDomainInOutConstraint { from = MkIntegerFrom fromInt, to = MkIntegerTo toInt }
  | (fromInt < toInteger (minBound :: Int64)) || (toInteger (maxBound :: Int64) < toInt) = Pointer
  | (fromInt < toInteger (minBound :: Int32)) || (toInteger (maxBound :: Int32) < toInt) = Bits 64
  | (fromInt < toInteger (minBound :: Int16)) || (toInteger (maxBound :: Int16) < toInt) = Bits 32
  | (fromInt < toInteger (minBound :: Int8)) || (toInteger (maxBound :: Int8) < toInt) = Bits 16
  | (fromInt < 0) || (1 < toInt) = Bits 8
  | otherwise                    = Bits 1
generateValueSize MkRangedFunctionDomainInOutConstraint { from = MkNegInfinite } = Pointer
generateValueSize MkRangedFunctionDomainInOutConstraint { to = MkPosInfinite }   = Pointer
