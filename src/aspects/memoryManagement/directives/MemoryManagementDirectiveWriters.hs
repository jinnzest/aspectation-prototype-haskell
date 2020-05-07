module MemoryManagementDirectiveWriters
  ( writeMemoryManagementDirectivesMap
  ) where

import Prelude ()
import Data.Text as T (Text, intercalate, concat, pack, empty, null)
import Data.Map (Map, toList)
import AspectsData as Asp (FunctionCall(MkFunctionCall, fcName, fcArgs))
import Data.Function (($), (.), const)
import AspectWriters (writeFunctionSignature, writeFunctionCall)
import Data.List (map, (!!), (++), length, reverse)
import Shared (justLookup, justGet, showT)
import Data.List.Index (indexed)
import Text.Show (Show(show))
import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (Maybe(Nothing), Maybe(Just), isJust)
import Data.Ord (Ord((>)))
import Data.Bool ((||))
import Data.Eq (Eq((==)))
import Location (Ranged(MkRanged, rItem))
import SemanticTree
  ( Statement
  , SemanticTree
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Return(Return, NoReturn)
  , unpackArg
  )
import AspectsShared (writeStatement, extractFunctions)
import CorePrelude (fromIntegral, Num((-)))
import Data.Map as M (Map)
import SemanticTree as Sem (FunctionSignature)
import Debug.Trace (trace)
import Data.Int (Int)
import MemoryManagementDirectivesData
  ( StatementMemoryManagementDirectives(MkStatementMemoryManagementDirectives, statementPos, statementMemoryManagement)
  , ExpressionMemoryManagementDirectives
    ( MkCallMemoryManagementDirectives
    , functionSignature
    , argumentsManagement
    , returnManagement
    , MkVariableMemoryManagementDirective
    )
  , MemoryManagementDirective(AllocHere, AllocAbove)
  )

writeMemoryManagementDirectivesMap
  :: SemanticTree -> Map FunctionSignature [StatementMemoryManagementDirectives] -> Text
writeMemoryManagementDirectivesMap tree directives =
  intercalate "\n" $ map (writeMemoryManagementDirective (extractFunctions tree)) $ toList directives

writeMemoryManagementDirective
  :: M.Map Sem.FunctionSignature [Ranged Statement]
  -> (FunctionSignature, [StatementMemoryManagementDirectives])
  -> Text
writeMemoryManagementDirective functions (sig, directives) =
  let function = justLookup sig functions
  in concat [writeFunctionSignature sig, " ~ ", writeResourceManagementDirectives (sig, function) directives]

writeResourceManagementDirectives
  :: (FunctionSignature, [Ranged Statement]) -> [StatementMemoryManagementDirectives] -> Text
writeResourceManagementDirectives function lineCallsMemoryManagementDirectives =
  concat $ map (writeLineCallsMemoryManagementDirectives function) lineCallsMemoryManagementDirectives

writeLineCallsMemoryManagementDirectives
  :: (FunctionSignature, [Ranged Statement]) -> StatementMemoryManagementDirectives -> Text
writeLineCallsMemoryManagementDirectives (sig, statements) MkStatementMemoryManagementDirectives { statementPos = callPos, statementMemoryManagement = callsMemoryManagement }
  = let pos = showT callPos
    in
      case callsMemoryManagement of
        [                  singleFuncCall] -> writeCallMemoryManagementDirectives pos singleFuncCall
        multipleFuncCalls@(_ : _         ) -> concat
          [ "\n|\t"
          , pos
          , " "
          , writeStatement (sig, let pos = fromIntegral (callPos - 1) in justGet statements pos)
          , concat $ map (writeCallMemoryManagementDirectives pos) callsMemoryManagement
          ]
        [] -> ""

writeCallMemoryManagementDirectives :: Text -> ExpressionMemoryManagementDirectives -> Text
writeCallMemoryManagementDirectives pos MkCallMemoryManagementDirectives { functionSignature = functionSignature@MkFunctionSignature { fsArgs = fsArgs, fsReturn = fsReturn }, argumentsManagement = argumentsManagement, returnManagement = returnManagement }
  = let
      directives =
        intercalate ", "
          $ let
              args =
                map (writeArgumentsManagement . first (\i -> unpackArg $ fsArgs !! i)) (toList argumentsManagement)
              ret = writeReturnManagement fsReturn returnManagement
            in if null ret then args else args ++ [ret]
      signature = writeFunctionSignature functionSignature
    in if null directives
      then concat ["\n|\t", pos, " ", signature]
      else concat ["\n\t", pos, " ", signature, " ~ ", directives]
writeCallMemoryManagementDirectives pos (MkVariableMemoryManagementDirective name directive) =
  concat [name, " = ", writeResourceManagementDirective directive]


writeReturnManagement (Return r) (Just directive) = concat [r, " = ", writeResourceManagementDirective directive]
writeReturnManagement (Return _) Nothing          = empty
writeReturnManagement NoReturn   _                = empty

writeArgumentsManagement :: (Text, MemoryManagementDirective) -> Text
writeArgumentsManagement (arg, directive) = concat [arg, " = ", writeResourceManagementDirective directive]

writeResourceManagementDirective :: MemoryManagementDirective -> Text
writeResourceManagementDirective AllocHere  = "here"
writeResourceManagementDirective AllocAbove = "up"
