module MemoryManagementDirectives
  ( defaultMemoryManagementDirective
  ) where

import MemoryManagementDirectivesData
  ( MemoryManagementDirectives
  , MemoryManagementDirective(AllocHere)
  , StatementMemoryManagementDirectives(statementPos, statementMemoryManagement, MkStatementMemoryManagementDirectives)
  , ExpressionMemoryManagementDirectives
    ( MkCallMemoryManagementDirectives
    , functionSignature
    , argumentsManagement
    , returnManagement
    , MkVariableMemoryManagementDirective
    )
  )
import Data.Map as M (union, singleton, fromList, map, elems, lookup, toList, Map)
import SemanticTree
  ( FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Return(NoReturn, Return)
  , Arg(MkValue)
  , SemanticTree
  , Function(MkFunction, fSignature, fBody)
  , Statement(MkNoAssignment, expression)
  , Expression(MkFunctionCall, fcsName, fcsArgs, MkConstantInteger, tmpVarName)
  , extractFunctions
  , tmpVarToReturn
  )
import Control.Monad.Except (Except)
import Errors (Errors)
import ValueSizeData (ValueSizeAnalytics, ValueSizeAnalyticsValues, ArgumentSizes)
import Location
import Data.List as L (map, length)
import Data.List.Index (indexed)
import Shared (justLookup)
import Data.Text as T

embeddedSignatures :: M.Map FunctionSignature FunctionSignature
embeddedSignatures =
  let print = MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn }
  in M.singleton print print

defaultMemoryManagementDirective
  :: ValueSizeAnalytics
  -> M.Map FunctionSignature FunctionSignature
  -> Function
  -> [StatementMemoryManagementDirectives]
defaultMemoryManagementDirective valueSizeAnalytics signatures function@MkFunction { fSignature, fBody = body } =
  let
    signaturesWithEmbedded     = signatures `union` embeddedSignatures
    functionValueSizeAnalytics = justLookup fSignature valueSizeAnalytics
  in L.map
    ( (\(p, s) -> MkStatementMemoryManagementDirectives
        { statementPos              = p
        , statementMemoryManagement =
          defaultMemoryManagementStatementDirective functionValueSizeAnalytics p function signaturesWithEmbedded
            $ rItem s
        }
      )
    . (\(p, v) -> (p + 1, v))
    )
    (indexed body)

defaultMemoryManagementStatementDirective
  :: Map ArgumentSizes ValueSizeAnalyticsValues
  -> Int
  -> Function
  -> M.Map FunctionSignature FunctionSignature
  -> Statement
  -> [ExpressionMemoryManagementDirectives]
defaultMemoryManagementStatementDirective valueSizeFunctionAnalytics _ _ signatures MkNoAssignment { expression = MkFunctionCall { fcsName = MkRanged { rItem = name }, fcsArgs = args, tmpVarName = tvn } }
  = let

      functionSignatures = M.fromList $ L.map (\(f, _) -> (f, f)) $ M.toList signatures
      signature          = MkFunctionSignature
        { fsName   = name
        , fsArgs   = L.map (const $ MkValue "v") args
        , fsReturn = tmpVarToReturn tvn
        }
    in
      [ MkCallMemoryManagementDirectives
          { functionSignature   = signature
          , argumentsManagement = M.fromList $ L.map (\(i, _) -> (i, AllocHere)) $ indexed args
          , returnManagement    = case tvn of
            Nothing -> Nothing
            Just _  -> Just AllocHere
          }
      ]
defaultMemoryManagementStatementDirective valueSizeFunctionAnalytics pos function@MkFunction { fSignature = MkFunctionSignature { fsReturn = ret }, fBody = body } _ MkNoAssignment { expression = MkConstantInteger _ c }
  = case ret of
    NoReturn -> []
    Return r ->
      let name = if pos == L.length body then r else T.concat ["c_", pack $ show c]
      in [MkVariableMemoryManagementDirective name AllocHere]
defaultMemoryManagementStatementDirective _ _ _ _ _ = []
