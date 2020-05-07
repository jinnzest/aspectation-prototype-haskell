module ExecFlowAnalyticsWriter where

import Prelude ()
import SemanticTree (SemanticTree, FunctionSignature)
import Data.Text as T (Text, intercalate, concat)
import ExecFlowAnalyticsData (ExecFlowAnalytics)
import Data.Map (toList)
import Data.Function (($), (.))
import Data.List as L (map, reverse, sort, concatMap, sortBy)
import AspectWriters (writeFunctionSignature)
import Data.Ord (Ord(compare))

writeExecFlowAnalytics :: ExecFlowAnalytics -> Text
writeExecFlowAnalytics analytics =
  intercalate "\n" $ map writeFunctionExecFlowAnalytics $ sortBy (\(l, _) (r, _) -> compare l r) $toList analytics

writeFunctionExecFlowAnalytics :: (FunctionSignature, [[FunctionSignature]]) -> Text
writeFunctionExecFlowAnalytics (sig, body) =
  concat [writeFunctionSignature sig, " ~ \n\t", writeFunctionBodyExecFlowAnalytics body]

writeFunctionBodyExecFlowAnalytics :: [[FunctionSignature]] -> Text
writeFunctionBodyExecFlowAnalytics items = intercalate "\n\t" $ map writeBodyItemExecFlowAnalytics items

writeBodyItemExecFlowAnalytics :: [FunctionSignature] -> Text
writeBodyItemExecFlowAnalytics = intercalate ", " . map writeFunctionSignature
