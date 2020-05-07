module MemoryManagementAnalyticsWriters
  ( writeMemoryManagementAnalyticsMap
  ) where

import Prelude (fromIntegral, Num((-)))
import Data.Text as T (Text, intercalate, concat, pack, empty, null)
import Data.Map (Map, toList)
import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs))
import Data.Function (($), (.), const)
import AspectWriters (writeFunctionSignature, writeFunctionCall)
import Data.List as L (map, (!!), (++), length, filter, sort, head, zip, zipWith)
import Shared (justLookup, justGet)
import Data.List.Index (indexed)
import Text.Show (Show(show))
import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (Maybe(Nothing), Maybe(Just), isJust)
import Data.Ord (Ord((>)))
import Data.Bool ((||), Bool(True))
import SemanticTree
  (FunctionSignature(fsReturn, MkFunctionSignature, fsName, fsArgs), SemanticTree, Statement, Arg(MkValue))
import Data.Map as M (toList, Map)
import AspectsShared (extractFunctions, writeStatement, writeExpression)
import SemanticTree as Sem (FunctionSignature)
import Location (Ranged(rItem))
import Data.Tuple (fst, curry)
import Data.Set as S (toList, size, member)
import Data.Eq (Eq((==)))
import MemoryManagementAnalyticsData
  ( MemoryManagement(MkMemoryManagement, toCallCases, lineMemoryManagement)
  , StatementMemoryManagement(callPos, resources, MkStatementMemoryManagement)
  , ResourceManagement(MkResourceManagement, action, resource)
  , Action(Alloc, Free)
  , Resource(MkArg, MkRet, MkVar, MkHiddenArg)
  , ToCallCases(MkToCallCases, hiddenArgs)
  )
import Data.Int (Int)
import qualified Data.Tuple as L

writeMemoryManagementAnalyticsMap :: SemanticTree -> Map FunctionSignature MemoryManagement -> Text
writeMemoryManagementAnalyticsMap tree analytics =
  intercalate "\n" $ sort $ map (writeMemoryManagementAnalytics (extractFunctions tree)) $ M.toList analytics

writeMemoryManagementAnalytics
  :: M.Map Sem.FunctionSignature [Ranged Statement] -> (FunctionSignature, MemoryManagement) -> Text
writeMemoryManagementAnalytics functions (sig, directives) =
  let function = justLookup sig functions
  in concat [writeFunctionSignature sig, " ~ \n", writeResourceManagementAnalyticsLines (sig, function) directives]

writeResourceManagementAnalyticsLines :: (FunctionSignature, [Ranged Statement]) -> MemoryManagement -> Text
writeResourceManagementAnalyticsLines function MkMemoryManagement { lineMemoryManagement = lineCallsMemoryManagementAnalytics, toCallCases = tcc }
  = intercalate "\n" (map (writeLineCallsMemoryManagementAnalytics tcc function) lineCallsMemoryManagementAnalytics)

writeLineCallsMemoryManagementAnalytics
  :: ToCallCases -> (FunctionSignature, [Ranged Statement]) -> StatementMemoryManagement -> Text
writeLineCallsMemoryManagementAnalytics toCallCases (sig, statements) MkStatementMemoryManagement { callPos = callPos, resources = resources }
  = concat $ L.zipWith (curry (writeResourceManagement toCallCases sig callPos)) statements resources

writeResourceManagement :: ToCallCases -> FunctionSignature -> Int -> (Ranged Statement, ResourceManagement) -> Text
writeResourceManagement toCallCases sig pos (statement, MkResourceManagement { action = a, resource = res }) =
  let
    allocPart = [ intercalate " " ["\talloc", writeResource toCallCases sig res] | S.member Alloc a ]
    freePart  = [ intercalate "" ["\tfree ", writeResource toCallCases sig res] | S.member Free a ]
  in intercalate "\n" $ allocPart ++ [concat ["\t", pack $show pos, " ", writeStatement (sig, statement)]] ++ freePart

writeResource :: ToCallCases -> FunctionSignature -> Resource -> Text
writeResource _ MkFunctionSignature { fsArgs = args, fsReturn = r } (MkArg       a) = let MkValue v = args !! a in v
writeResource _ sig@MkFunctionSignature { fsArgs = args, fsReturn = fsRet } (MkRet       r) = writeExpression (sig, r)
writeResource _ _ (MkVar       v) = v

writeResource MkToCallCases { hiddenArgs = ha } MkFunctionSignature { fsArgs = args, fsReturn = r } (MkHiddenArg a) = a

