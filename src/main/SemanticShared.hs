module SemanticShared
  ( writeStatement
  ) where

import Prelude ()
import SemanticTree
  ( Expression(MkConstantInteger, MkFunctionArgument, fcsName, fcsArgs, tmpVarName)
  , FunctionSignature(MkFunctionSignature, fsArgs)
  , unpackArg
  , Statement(MkNoAssignment)
  )
import Location (Ranged(MkRanged, rItem))
import Data.List as L (length, map, (++))
import qualified SemanticTree as Sem
import Shared (showT)
import Panic (panic)
import Data.Text (Text, unwords)
import Text.Show (Show(show))
import Data.Ord ((>=))
import Data.Function (($))
import Data.List ((!!))

writeStatement :: (FunctionSignature, Ranged Statement) -> Text
writeStatement (sig, MkRanged { rItem = MkNoAssignment { Sem.expression = expr } }) = writeExpression (sig, expr)

writeExpression (_, MkConstantInteger _ MkRanged { rItem = v }) = showT v
writeExpression (MkFunctionSignature { fsArgs = args }, MkFunctionArgument _ MkRanged { rItem = v }) =
  if v >= L.length args then panic ("\nargs: " ++ show args ++ "\nv: " ++ show v) else unpackArg $ args !! v
writeExpression (sig, Sem.MkFunctionCall { fcsName = name, fcsArgs = args, tmpVarName = tvn }) =
  unwords (rItem name : map (\a -> writeExpression (sig, a)) args)
