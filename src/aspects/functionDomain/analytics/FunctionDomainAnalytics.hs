module FunctionDomainAnalytics
  ( Context
  , generateFunctionDomainAnalytics
  , embeddedFunctionDomainAnalytics
  ) where

import Prelude (Integer, Num((-), (+)))
import Text.Show (Show(show))
import Data.Int (Int)
import Data.Bool (Bool(True, False), not, (&&), (||), otherwise)
import Data.List (map, concatMap, (++), reverse)
import Data.Tuple (snd, fst)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Eq (Eq((==)))
import Data.Ord (Ord((<=), (>), (>=), (<)))
import LLVM.Prelude (Integral(toInteger))
import Control.Monad (Monad(return), when, foldM, filterM, mapM_, mapM)
import Control.Monad.Except (ExceptT, Except, liftIO, throwError, when)
import Data.Map as M (Map, empty, fromList, lookup, toList, member, insert, singleton)
import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust, isJust, catMaybes)
import Data.Sort (sortBy)
import Data.Text (Text, append, pack, unpack)
import Data.List as L (concat, last, filter, find, head, (!!))
import Data.List.Index (indexed)
import Data.Text.IO (putStrLn, writeFile)
import Debug.Trace (trace)
import Data.Foldable (sequence_, Foldable(foldl, elem))
import Data.Either (Either(Left, Right), isLeft, isRight, rights)
import Data.Set as S (Set, difference, fromList, toList, insert, empty)
import Text.Megaparsec (SourcePos(SourcePos), unPos, sourceLine)
import GHC.Generics (Generic)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()
import Control.Monad.State.Lazy (State, get, put, runState, modify)

import Location as Loc (Ranged(MkRanged), rItem, range, from)
import AspectsData as Asp (Analytics, FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import AspectsParserIO (parseAspectsOnPath)
import AspectsShared (extractLine)
import AspectWriters (writeFunctionSignature)
import Errors (Errors(MkErrors), Error(MkError))
import FunctionDomainDirectives (FunctionDomainDirectives, lessToFrom)
import FunctionDomainDirectivesData as Asp
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , from
  , to
  , input
  , output
  , FunctionDomainConstraintFrom(MkNegInfinite, MkIntegerFrom)
  , FunctionDomainConstraintTo(MkPosInfinite, MkIntegerTo)
  )
import ParserWrapperIO (parsePath)
import SemanticTree
  ( Function(MkFunction, fBody, fSignature)
  , NameArgs(naArgs)
  , SemanticTree
  , _semanticTree
  , unpackArg
  , Expression(tmpVarName)
  )
import Shared (justLookup)
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName, tmpVarName)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , Return(NoReturn, Return)
  , Statement(MkNoAssignment, expression)
  , Function
  , fsArgs
  , fBody
  , fSignature
  )
import FunctionDomainAnalyticsData
  ( UndefinedProbabilityAnalytics
  , FunctionDomainAnalytics
  , FunctionDomainAnalytics(MkFunctionDomainAnalytics)
  , FunctionDomainAnalytics(_undefinedProbabilities)
  , FunctionDomainAnalytics(_constraints)
  , UndefinedFunctionProbability(MkUndefinedFunctionProbability, callUndefinedProbabilities, resultUndefinedProbability)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, functionCall, callProbability)
  , UndefinedProbability(Defined, MaybeDefined)
  , CallProbability(MaybeWillBeCalled, WillBeCalled)
  )
import Panic (panic)
import BasicPrelude (fromIntegral)
import FunctionDomainDirectivesParser (defaultConstraint)
import ShowJ (showJ)

data Context = MkContext
  { functions          :: Map Sem.FunctionSignature [Ranged Statement]
  , functionSignatures :: Map Text Sem.FunctionSignature
  , analytics          :: UndefinedProbabilityAnalytics
  , callers            :: Set Sem.FunctionSignature
  , directives         :: FunctionDomainDirectives
  }
  deriving Generic
  deriving ToJSON via Vanilla Context

instance Show Context where
  show = showJ

embeddedFunctionDomainAnalytics :: UndefinedProbabilityAnalytics
embeddedFunctionDomainAnalytics =
  let printSignature = MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn }
  in
    M.singleton
      printSignature
      MkUndefinedFunctionProbability { callUndefinedProbabilities = S.empty, resultUndefinedProbability = Nothing }

generateFunctionDomainAnalytics
  :: [Sem.FunctionSignature]
  -> SemanticTree
  -> FunctionDomainDirectives
  -> UndefinedProbabilityAnalytics
  -> UndefinedProbabilityAnalytics
generateFunctionDomainAnalytics embeddedFuncs coreTree directives loadedUndefinedProbabilities =
  let
    signatures      = map (\MkFunction { fSignature } -> fSignature) $ concatMap snd $ M.toList $ _semanticTree coreTree
    constraintsList = map (\sig -> (sig, justLookup sig directives)) signatures
    _constraints    = M.fromList constraintsList
    _undefinedProbabilities =
      generateUndefinedProbabilities embeddedFuncs coreTree directives loadedUndefinedProbabilities
  in _undefinedProbabilities

generateUndefinedProbabilities
  :: [Sem.FunctionSignature]
  -> SemanticTree
  -> FunctionDomainDirectives
  -> UndefinedProbabilityAnalytics
  -> UndefinedProbabilityAnalytics
generateUndefinedProbabilities embeddedFunctions coreTree directives analytics =
  let
    functions =
      M.fromList $ map (\MkFunction { fSignature = fSignature, fBody = body } -> (fSignature, body)) $ concatMap
        snd
        (M.toList $ _semanticTree coreTree)
    functionSignatures =
      M.fromList
        $  map (\sig@Sem.MkFunctionSignature { Sem.fsName = fsName } -> (fsName, sig))
        $  embeddedFunctions
        ++ map fst (M.toList functions)
    callers = S.empty
  in fst $ runState
    generateUndefinedProbabilitiesWithContext
    MkContext { functions, functionSignatures, analytics, callers, directives }

generateUndefinedProbabilitiesWithContext :: State Context UndefinedProbabilityAnalytics
generateUndefinedProbabilitiesWithContext = do
  context <- get
  let functionSignaturesAndBodies = M.toList $ functions context
  mapM_ generateUndefinedProbabilitiesFunction functionSignaturesAndBodies
  analytics <$> get

generateUndefinedProbabilitiesFunction :: (Sem.FunctionSignature, [Ranged Statement]) -> State Context ()
generateUndefinedProbabilitiesFunction (signature, statements) = do
  context <- get
  let fistLinePos = extractLine $ range $ head statements
  let args        = map unpackArg $fsArgs signature
  let funcName    = fsName signature
  if M.member signature $ analytics context
    then return ()
    else do
      probability <- generateUndefinedProbabilitiesFunctionBody funcName fistLinePos args $ map rItem statements
      modify
        (\newContext -> MkContext
          { functions          = functions newContext
          , directives         = directives newContext
          , callers            = callers newContext
          , analytics          = M.insert signature probability $ analytics newContext
          , functionSignatures = functionSignatures newContext
          }
        )
      return ()

generateUndefinedProbabilitiesFunctionBody
  :: Text -> Integer -> [Text] -> [Statement] -> State Context UndefinedFunctionProbability
generateUndefinedProbabilitiesFunctionBody funcName fistLinePos args statements = do
  statementsWithProbabilities <- foldM
    (\probabilities s -> do
      p <- generateUndefinedProbabilitiesFunctionStatement funcName fistLinePos s
      return $ p : probabilities
    )
    []
    statements
  let callUndefinedProbabilities = S.fromList $ rights statementsWithProbabilities
  let lastStatement              = last statementsWithProbabilities
  context <- get
  let signature = justLookup funcName $ functionSignatures context
  let
    resultUndefinedProbability = case fsReturn signature of
      NoReturn -> Nothing
      Return _ -> case lastStatement of
        Left  v -> Just Defined
        Right v -> case find (== MaybeWillBeCalled) $ map callProbability $ S.toList $ functionCalls v of
          Nothing -> Just Defined
          Just _  -> Just MaybeDefined
  return MkUndefinedFunctionProbability { callUndefinedProbabilities, resultUndefinedProbability }

generateUndefinedProbabilitiesFunctionStatement
  :: Text -> Integer -> Statement -> State Context (Either UndefinedProbability PositionedFunctionCall)
generateUndefinedProbabilitiesFunctionStatement funcName firstLinePos MkNoAssignment { expression = expr@Sem.MkFunctionCall { fcsName = MkRanged { rItem = fcsName, range = range }, fcsArgs = fcsArgs } }
  = do
    let callPos = fromIntegral $ extractLine range - firstLinePos + 1
    functionCallsList <- genFunctionCallWithProbability funcName [] expr
    let functionCalls = S.fromList functionCallsList
    return $ Right $ MkPositionedFunctionCall { callPos, functionCalls }

generateUndefinedProbabilitiesFunctionStatement funcName _ MkNoAssignment { expression = Sem.MkConstantInteger _ _ } =
  return $ Left Defined
generateUndefinedProbabilitiesFunctionStatement funcName _ MkNoAssignment { expression = Sem.MkFunctionArgument _ _ } =
  return $ Left Defined

genFunctionCallWithProbability
  :: Text -> [FunctionCallWithProbability] -> Expression -> State Context [FunctionCallWithProbability]
genFunctionCallWithProbability funcName callsAcc Sem.MkFunctionCall { fcsName = MkRanged { rItem = fcName, range = range }, fcsArgs = fcsArgs, Sem.tmpVarName = tvn }
  = do
    context <- get
    res     <- mapM (genCallProbabilityByArgumentDomain funcName fcName) (indexed fcsArgs)
    let isFunctionDefinedByArgumentDomains = not $ hasMaybeDefined $ catMaybes res
    subFunctionResults <- foldM (genFunctionCallWithProbability funcName) callsAcc fcsArgs
    let subFunctionResultProbabilities        = map toResultProbability subFunctionResults
    let isFunctionDefinedBySubFunctionResults = not $ hasMaybeDefined subFunctionResultProbabilities
    let
      callProbabilityArg = if isFunctionDefinedByArgumentDomains && isFunctionDefinedBySubFunctionResults
        then WillBeCalled
        else MaybeWillBeCalled
    let fcArgs = map (\(MkValue a) -> a) $ Sem.fsArgs $ justLookup fcName $ functionSignatures context
    let
      newCallsAcc =
        MkFunctionCallWithProbability
            { functionCall    = Asp.MkFunctionCall { Asp.fcName, Asp.fcArgs, Asp.tmpVarName = tvn }
            , callProbability = callProbabilityArg
            }
          : subFunctionResults
    return newCallsAcc

genFunctionCallWithProbability fn _ (MkConstantInteger  _ _) = return []

genFunctionCallWithProbability fn _ (MkFunctionArgument _ _) = return []

toResultProbability :: FunctionCallWithProbability -> UndefinedProbability
toResultProbability = callToResultProbability . callProbability

callToResultProbability :: CallProbability -> UndefinedProbability
callToResultProbability MaybeWillBeCalled = MaybeDefined
callToResultProbability WillBeCalled      = Defined

genCallProbabilityByArgumentDomain :: Text -> Text -> (Int, Expression) -> State Context (Maybe UndefinedProbability)
genCallProbabilityByArgumentDomain callerFuncName funcNameToCall (pos, MkFunctionArgument _ MkRanged { rItem = a }) =
  do
    context <- get
    let callerFuncSig    = justLookup callerFuncName $ functionSignatures context
    let inputDirective   = input $ justLookup callerFuncSig $ directives context
    let callerDomain     = justLookup a inputDirective
    let funcToCallSig    = justLookup funcNameToCall $ functionSignatures context
    let outputDirective  = input $ justLookup funcToCallSig $ directives context
    let funcToCallDomain = justLookup pos outputDirective
    return $ Just $ isArgDefined callerDomain funcToCallDomain

genCallProbabilityByArgumentDomain callerFuncName funcNameToCall (pos, MkConstantInteger _ MkRanged { rItem = c }) = do
  context <- get
  let
    funcToCallSig    = justLookup funcNameToCall $ functionSignatures context
    inputDirective   = input $ justLookup funcToCallSig $ directives context
    funcToCallDomain = justLookup pos inputDirective
  return $ if elem Defined $map (`isConstDefined` c) funcToCallDomain then Just MaybeDefined else Just Defined

genCallProbabilityByArgumentDomain _ _ (_, Sem.MkFunctionCall{}) = return Nothing

isConstDefined :: FunctionDomainInOutConstraints -> Integer -> UndefinedProbability
isConstDefined MkRangedFunctionDomainInOutConstraint { Asp.from = MkNegInfinite, Asp.to = MkPosInfinite } const =
  Defined
isConstDefined MkRangedFunctionDomainInOutConstraint { Asp.from = MkNegInfinite, Asp.to = MkIntegerTo valueTo } const =
  if const <= valueTo then Defined else MaybeDefined
isConstDefined MkRangedFunctionDomainInOutConstraint { Asp.from = MkIntegerFrom valueFrom, Asp.to = MkPosInfinite } const
  = if const >= valueFrom then Defined else MaybeDefined
isConstDefined MkRangedFunctionDomainInOutConstraint { Asp.from = MkIntegerFrom valueFrom, Asp.to = MkIntegerTo valueTo } const
  = if const >= valueFrom && const <= valueTo then Defined else MaybeDefined

isArgDefined :: [FunctionDomainInOutConstraints] -> [FunctionDomainInOutConstraints] -> UndefinedProbability
isArgDefined []      []      = panic "there is no caller nor to call constraints"
isArgDefined (x : _) []      = MaybeDefined
isArgDefined []      (x : _) = Defined
isArgDefined caller@(callerHead : callerTail) toCall@(toCallHead : toCallTail)
  | (Asp.from callerHead < Asp.from toCallHead) || (to callerHead > to toCallHead) = MaybeDefined
  | lessToFrom (Asp.to toCallHead) (Asp.from callerHead) = isArgDefined caller toCallTail
  | otherwise = isArgDefined callerTail toCall

hasMaybeDefined :: [UndefinedProbability] -> Bool
hasMaybeDefined probabilities = isJust $ find (== MaybeDefined) probabilities
