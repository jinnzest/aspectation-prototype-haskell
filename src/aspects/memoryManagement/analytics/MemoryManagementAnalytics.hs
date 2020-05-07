module MemoryManagementAnalytics
  ( generateMemoryManagementAnalytics
  ) where

import Control.Monad.Except (ExceptT, liftIO)
import Data.Map (Map, empty)
import Data.Text.IO (putStrLn)
import Prelude (IO, Maybe, ($), return)

import AspectsData (Analytics)
import Errors (Errors)
import SemanticTree
  ( SemanticTree
  , Statement(MkNoAssignment, expression)
  , Function(fSignature, fBody, MkFunction)
  , _semanticTree
  , unpackArg
  , Expression(fcsName, fcsArgs, tmpVarName)
  )
import Data.Text (Text)
import MemoryManagementAnalyticsData
  ( MemoryManagementAnalytics
  , MemoryManagement(MkMemoryManagement, toCallCases, lineMemoryManagement)
  , ToCallCases(MkToCallCases, toCallArgs, toCallRet, hiddenArgs)
  , StatementMemoryManagement(MkStatementMemoryManagement, resources, callPos)
  , ResourceManagement(MkResourceManagement, action, resource)
  , Resource(MkVar, MkArg, MkHiddenArg, MkRet)
  , Action(Alloc, Free)
  )
import MemoryManagementDirectivesData
  ( MemoryManagementDirectives
  , StatementMemoryManagementDirectives(statementPos, statementMemoryManagement)
  , ExpressionMemoryManagementDirectives(MkVariableMemoryManagementDirective)
  , MemoryManagementDirective(AllocHere, AllocAbove)
  )
import ExecFlowAnalyticsData (ExecFlowAnalytics)
import SemanticTree as Sem
  ( FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall)
  )
import Location (Ranged(range, rItem, MkRanged))
import Data.Set as S (Set, empty, fromList, insert, size, member)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import Deriving.Aeson.Stock (Vanilla)
import Data.Aeson (ToJSON, toJSON, Value(String))
import Data.Aeson.QQ ()
import Data.Map as M (fromList, toList, empty, member, insert, lookup, size)
import Data.List
import Data.Tuple (snd, fst)
import Control.Monad.State (runState, State, evalState, get, mapM_, gets, modify, mapM)
import Data.Function ((.))
import Data.Functor ((<$>))
import AspectsShared (extractLine)
import BasicPrelude (Integer, Num((+)), Num((-)))
import Data.Either (Either)
import ValueSizeData (ValueSizeAnalytics)
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.List.Index (indexed)
import Data.Int (Int)
import Shared (justLookup, genNextIdentifier)
import Text.Show (Show(show))
import Data.Eq (Eq((==), (/=)))
import Panic (panic)
import qualified Data.Text as T
import Data.Bool (not)

data Context = MkContext
  { functions            :: Map Sem.FunctionSignature [Ranged Statement]
  , functionSignatures   :: Map Text Sem.FunctionSignature
  , analytics            :: MemoryManagementAnalytics
  , callers              :: Set Sem.FunctionSignature
  , directives           :: MemoryManagementDirectives
  , valueSizeAnalytics   :: ValueSizeAnalytics
  , execFlowAnalytics    :: ExecFlowAnalytics
  , generatedIdentifiers :: S.Set Text
  }
  deriving Generic
  deriving ToJSON via Vanilla Context

generateMemoryManagementAnalytics
  :: (ValueSizeAnalytics, ExecFlowAnalytics)
  -> SemanticTree
  -> MemoryManagementDirectives
  -> MemoryManagementAnalytics
  -> MemoryManagementAnalytics
generateMemoryManagementAnalytics (valueSizeAnalytics, execFlowAnalytics) coreTree directives analytics =
  let
    functions =
      M.fromList $ map (\MkFunction { fSignature = fSignature, fBody = body } -> (fSignature, body)) $ concatMap
        snd
        (M.toList $ _semanticTree coreTree)
    functionSignatures = M.fromList
      $ map ((\sig@Sem.MkFunctionSignature { Sem.fsName = fsName } -> (fsName, sig)) . fst) (M.toList functions)
    callers = S.empty
  in evalState
    generateMemoryManagementAnalyticsWithContext
    MkContext
      { functions
      , functionSignatures
      , analytics
      , callers
      , directives
      , valueSizeAnalytics
      , execFlowAnalytics
      , generatedIdentifiers = S.empty
      }

generateMemoryManagementAnalyticsWithContext :: State Context MemoryManagementAnalytics
generateMemoryManagementAnalyticsWithContext = do
  context <- get
  let functionSignaturesAndBodies = M.toList $ functions context
  mapM_ generateMemoryManagementFunction functionSignaturesAndBodies
  gets analytics

generateMemoryManagementFunction :: (Sem.FunctionSignature, [Ranged Statement]) -> State Context ()
generateMemoryManagementFunction (signature, statements) = do
  context <- get
  let args     = map unpackArg $fsArgs signature
  let funcName = fsName signature
  if M.member signature $ analytics context
    then return ()
    else do
      lineMemoryManagement <-
        generateMemoryManagementFunctionBody signature $ map (\(i, s) -> (i + 1, s)) $ indexed $ map rItem statements
      modify
        (\newContext -> MkContext
          { functions            = functions newContext
          , directives           = directives newContext
          , callers              = callers newContext
          , analytics            =
            let
              resourcesManagement = case M.lookup signature $ analytics newContext of
                Nothing -> MkMemoryManagement
                  { toCallCases          = MkToCallCases { toCallArgs = M.empty, toCallRet = S.empty, hiddenArgs = [] }
                  , lineMemoryManagement = lineMemoryManagement
                  }
                Just a ->
                  MkMemoryManagement { toCallCases = toCallCases a, lineMemoryManagement = lineMemoryManagement }
            in M.insert signature resourcesManagement $ analytics newContext
          , functionSignatures   = functionSignatures newContext
          , valueSizeAnalytics   = valueSizeAnalytics newContext
          , execFlowAnalytics    = execFlowAnalytics newContext
          , generatedIdentifiers = S.empty
          }
        )
      return ()

generateMemoryManagementFunctionBody
  :: Sem.FunctionSignature -> [(Int, Statement)] -> State Context [StatementMemoryManagement]
generateMemoryManagementFunctionBody signature = mapM (generateMemoryManagementFunctionStatement signature)

generateMemoryManagementFunctionStatement
  :: Sem.FunctionSignature -> (Int, Statement) -> State Context StatementMemoryManagement
generateMemoryManagementFunctionStatement signature (callPos, MkNoAssignment { expression = expr@Sem.MkFunctionCall { fcsName = MkRanged { rItem = fcsName, range = range }, fcsArgs = fcsArgs, tmpVarName = tvn } })
  = do
    context <- get
    let functionDirectives = justLookup signature $ directives context
    let action             = S.fromList [Alloc]
    return MkStatementMemoryManagement
      { callPos
      , resources = case tvn of
        Nothing -> []
        Just r  -> [MkResourceManagement { action, resource = MkVar r }]
      }

generateMemoryManagementFunctionStatement signature (callPos, MkNoAssignment { expression = Sem.MkConstantInteger _ c })
  = do
    context <- get
    let functionDirectives = justLookup signature $ directives context
    let functionAnalytics  = justLookup signature $ analytics context
    let statementDirective = functionDirectives !! (callPos - 1)
    let statements         = justLookup signature $ functions context
    let statementsAmount   = length statements
    let lastStatement      = statementsAmount == callPos
    let action             = S.fromList [Alloc]
    hiddenArg <- genNextIdentifierCtx
    if statementPos statementDirective /= callPos
      then panic
        ("Internal error: incorrect directive position. Expected " ++ show callPos ++ " but got " ++ show
          (statementPos statementDirective)
        )
      else case head $ statementMemoryManagement statementDirective of
        MkVariableMemoryManagementDirective name AllocHere -> return MkStatementMemoryManagement
          { callPos
          , resources =
            [ MkResourceManagement
                { action
                , resource = if lastStatement then MkRet $ extractExpression $ rItem $ last statements else MkVar name
                }
            ]
          }
        MkVariableMemoryManagementDirective name AllocAbove -> do
          let lHiddenArgs = hiddenArgs $ toCallCases functionAnalytics

          modify
            (\newContext -> MkContext
              { functions            = functions newContext
              , functionSignatures   = functionSignatures newContext
              , analytics            =
                let
                  mm  = justLookup signature $ analytics newContext
                  tCA = toCallArgs $ toCallCases mm
                  tCR = toCallRet $ toCallCases mm
                in
                  M.insert
                      signature
                      MkMemoryManagement
                        { toCallCases          = MkToCallCases
                          { toCallArgs = tCA
                          , toCallRet  = tCR
                          , hiddenArgs = hiddenArg : lHiddenArgs
                          }
                        , lineMemoryManagement = lineMemoryManagement mm
                        }
                    $ analytics newContext
              , callers              = callers newContext
              , directives           = directives newContext
              , valueSizeAnalytics   = valueSizeAnalytics newContext
              , execFlowAnalytics    = execFlowAnalytics newContext
              , generatedIdentifiers = generatedIdentifiers newContext
              }
            )
          return MkStatementMemoryManagement
            { callPos
            , resources = [MkResourceManagement { action, resource = MkHiddenArg hiddenArg }]
            }
        _ -> return MkStatementMemoryManagement { callPos, resources = [] }

generateMemoryManagementFunctionStatement signature (callPos, MkNoAssignment { expression = Sem.MkFunctionArgument _ _ })
  = return MkStatementMemoryManagement { callPos, resources = [] }


genNextIdentifierCtx :: State Context Text
genNextIdentifierCtx = gets (genNextIdentifier . generatedIdentifiers)

extractExpression :: Statement -> Expression
extractExpression (MkNoAssignment expr) = expr
