module ExecFLowDirectivesWriter where

import Prelude ()
import SemanticTree (SemanticTree, FunctionSignature)
import Data.Text as T (Text, intercalate, concat)
import ExecFlowDirectivesData (ExecFlowDirectives, Visibility(Pub, Priv))
import Data.Map (toList)
import Data.Function (($), (.))
import Data.List as L (map, reverse, sort, concatMap, sortBy)
import AspectWriters (writeFunctionSignature)
import Data.Ord (Ord(compare))

writeExecFlowDirectives :: ExecFlowDirectives -> Text
writeExecFlowDirectives directives =
  intercalate "\n" $ map writeFunctionExecFlowDirectives $ sortBy (\(l, _) (r, _) -> compare l r) $ toList directives

writeFunctionExecFlowDirectives :: (FunctionSignature, Visibility) -> Text
writeFunctionExecFlowDirectives (sig, body) =
  concat [writeFunctionSignature sig, " ~ ", writeFunctionBodyExecFlowDirectives body]

writeFunctionBodyExecFlowDirectives :: Visibility -> Text
writeFunctionBodyExecFlowDirectives Pub  = "pub"
writeFunctionBodyExecFlowDirectives Priv = "priv"

