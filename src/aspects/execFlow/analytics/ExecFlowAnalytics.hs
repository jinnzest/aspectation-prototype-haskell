module ExecFlowAnalytics
  ( generateExecFlowAnalytics
  ) where

import Prelude ()
import SemanticTree
  ( SemanticTree
  , FunctionSignature
  , Function(MkFunction, fSignature, fBody)
  , _semanticTree
  , Statement
  , Arg(MkValue)
  , tmpVarToReturn
  , NameArgs(MkNameArgs, naName, naArgs)
  )
import ExecFlowAnalyticsData (ExecFlowAnalytics)
import Control.Monad.Except (ExceptT)
import Errors (Errors)
import Data.Map (Map, empty)
import Location (Ranged(range, rItem))
import Data.List as L (map, concatMap, head, concat, null, sort, length, groupBy, filter, tail, (++), null, filter)
import Data.Text (Text)
import Data.Set (Set, singleton)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()
import Data.Map as M (Map, fromList, toList, empty, member, insert, lookup)
import SemanticTree as Sem
  ( SemanticTree(_semanticTree)
  , Expression(..)
  , Return(Return, NoReturn)
  , Function(MkFunction, fSignature, fBody)
  , Statement(..)
  , FunctionSignature(..)
  )
import Control.Monad.State.Lazy (evalState, State, runState, MonadState(get), mapM_, modify, mapM)
import Data.Set as S (Set, empty, fromList, member, singleton, toList)
import Data.Function (($), (.), const)
import Data.Tuple (snd, fst, uncurry)
import BasicPrelude (error)
import Data.Functor ((<$>))
import AspectsShared (extractLine)
import Control.Monad (Monad(return))
import CorePrelude (Integer)
import AspectsData (FunctionCall(fcName, fcArgs))
import Data.Maybe (Maybe(Just, Nothing))
import Debug.Trace (trace)
import Text.Show (Show(show))
import Data.Ord (Ord((>)))
import Data.Eq (Eq((==)))
import Data.Bool ((&&), Bool(False, True))
import ExecFlowDirectivesData (ExecFlowDirectives, Visibility(Pub))
import Shared (justLookup)


data Context = MkContext
  { functions          :: M.Map FunctionSignature [Ranged Statement]
  , functionSignatures :: Map NameArgs FunctionSignature
  , analytics          :: ExecFlowAnalytics
  , callers            :: Set FunctionSignature
  }
  deriving (Generic, Show)
  deriving ToJSON via Vanilla Context

generateExecFlowAnalytics :: SemanticTree -> ExecFlowDirectives -> ExecFlowAnalytics
generateExecFlowAnalytics tree directives =
  let
    functions =
      M.fromList $ L.map (\MkFunction { fSignature = fSignature, fBody = body } -> (fSignature, body)) $ concatMap
        snd
        (M.toList $ _semanticTree tree)
    functionSignatures = M.fromList $ L.map
      ( (\sig@Sem.MkFunctionSignature { Sem.fsName = naName, Sem.fsArgs = naArgs } ->
          (MkNameArgs { naName, naArgs }, sig)
        )
      . fst
      )
      (M.toList directives)
    callers     = S.empty
    nonFilteted = -- trace ("\nfunctionSignatures: "++show functionSignatures) 
                  fst $ runState
      generateExecFlowAnalyticsWithContext
      MkContext { functions, functionSignatures, analytics = M.empty, callers = S.empty }
    filtered = filterOutPrivFunctions nonFilteted directives
  in filtered

filterOutPrivFunctions :: ExecFlowAnalytics -> ExecFlowDirectives -> ExecFlowAnalytics
filterOutPrivFunctions analytics directives =
  M.fromList
    $ L.filter
        (\(sig, _) -> case M.lookup sig directives of
          Nothing -> -- trace ("\nnot found directive for signature: " ++ show sig) 
            False
          Just d -> if d == Pub then True else False
        )
    $ M.toList analytics


generateExecFlowAnalyticsWithContext :: State Context ExecFlowAnalytics
generateExecFlowAnalyticsWithContext = do
  context <- get
  let
    functionSignaturesAndBodies = -- trace ("\ncontext: " ++ show context) 
      M.toList $ functions context
  mapM_ generateExecFlowAnalyticsFunction functionSignaturesAndBodies
  analytics <$> get

generateExecFlowAnalyticsFunction :: (FunctionSignature, [Ranged Statement]) -> State Context ()
generateExecFlowAnalyticsFunction (signature, statements) = do
  context <- -- trace ("\ngen for signature: " ++ show signature ++ "\nstatements: " ++ show statements) 
             get
  modify
    (\newContext -> MkContext
      { functions          = functions newContext
      , callers            = S.singleton signature
      , analytics          = analytics newContext
      , functionSignatures = functionSignatures newContext
      }
    )
  let args     = fsArgs signature
  let funcName = fsName signature
  if M.member signature $ analytics context
    then -- trace ("\n\nempty for signature: " ++ show signature) 
         return ()
    else do
      --trace ("\n\nnon empty for signature: " ++ show signature) $ 
      generateExecFlowFunctionBody signature $ L.map rItem statements
      return ()

generateExecFlowFunctionBody :: FunctionSignature -> [Statement] -> State Context [[FunctionSignature]]
generateExecFlowFunctionBody signature statements = do
  modify
    (\newContext -> MkContext
      { functions          = functions newContext
      , callers            = S.singleton signature
      , analytics          = analytics newContext
      , functionSignatures = functionSignatures newContext
      }
    )
  statementsExecFlow <- mapM generateExecFlowStatement statements
  let beingCalledFuncs = S.toList $ S.fromList $ concat statementsExecFlow
  loadedFuncs <- mapM loadSubFuncs beingCalledFuncs
  let flattenFuncs = sort $ S.toList $ S.fromList $ concat loadedFuncs
  modify
    (\newContext -> MkContext
      { functions          = functions newContext
      , callers            = callers newContext
      , analytics          =
        -- trace
        --     (  "\n\ninsert sig: "
        --     ++ show signature
        --     ++ "\nbeingCalledFuncs: "
        --     ++ show beingCalledFuncs
        --     ++ "\nstatementsExecFlow: "
        --     ++ show statementsExecFlow
        --     ++ "\nloadedFuncs: "
        --     ++ show flattenFuncs
        --     )
                             M.insert signature flattenFuncs $ analytics newContext
      , functionSignatures = functionSignatures newContext
      }
    )
  return -- $ trace
    -- (  "\n\nsignature: "
    -- ++ show signature
    -- ++ "\nbeingCalledFuncs: "
    -- ++ show beingCalledFuncs
    -- ++ "\nloadedFuncs: "
    -- ++ show flattenFuncs
    -- )
         flattenFuncs

loadSubFuncs :: FunctionSignature -> State Context [[FunctionSignature]]
loadSubFuncs signature = do
  ctx <- get
  let altcs = analytics ctx
  let fncs  = functions ctx
  signatures <- case M.lookup signature altcs of
    Nothing -> case M.lookup signature fncs of
      Nothing -> -- trace ("\nNeither body nor analytics for function: " ++ show signature) 
        return []
      Just b -> generateExecFlowFunctionBody signature (L.map rItem b)
    Just f -> -- trace ("\n\nFound analytics for function: " ++ show signature ++ "\nanalytics: " ++ show f) 
      return f
  let loadedSignatures = L.map (signature :) signatures
  let allSignatures = if L.null loadedSignatures then [[signature]] else loadedSignatures
  return -- $ trace
    -- (  "\n\nloadSubFuncs signature: "
    -- ++ show signature
    -- ++ "\nsignatures: "
    -- ++ show signatures
    -- ++ "\nallSignatures:"
    -- ++ show allSignatures
    -- )
         allSignatures

generateExecFlowStatement :: Statement -> State Context [FunctionSignature]
generateExecFlowStatement MkNoAssignment { expression = expr } = generateExecFlowExpression expr

generateExecFlowExpression :: Expression -> State Context [FunctionSignature]
generateExecFlowExpression (MkConstantInteger  _ _)                          = return []
generateExecFlowExpression (MkFunctionArgument _ _)                          = return []
generateExecFlowExpression MkFunctionCall { fcsName = name, fcsArgs = args } = do
  ctx <- get
  let
    nameArgs  = MkNameArgs { naName = rItem name, naArgs = L.map (const $ MkValue "v") args }
    signature = justLookup nameArgs $ functionSignatures ctx
  if S.member signature (callers ctx)
    then return -- $trace ("signature is member of callers: " ++ show signature) 
                []
    else do
      expressionsExecFlow <- mapM generateExecFlowExpression args
      let res = signature : concat expressionsExecFlow
      return -- $ trace ("\nEXPR: " ++ show res) 
             res
