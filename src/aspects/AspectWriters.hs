module AspectWriters
  ( writeFunctionSignature
  , writeFunctionCall
  ) where

import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.Text (Text, append, concat, intercalate, null, snoc)
import Prelude ()
import Debug.Trace (trace)
import Data.List ((++), map, filter)
import SemanticTree
  (FunctionSignature(fsName, fsArgs, fsReturn, MkFunctionSignature), showArg, Arg(MkValue), Return(Return, NoReturn))
import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import Data.Function (($), (.))
import Data.Bool (not)

writeFunctionSignature :: FunctionSignature -> Text
writeFunctionSignature sig = intercalate " "
  $ filter (not . null) [fsName sig, intercalate " " $ filter (not . null) [writeArgs sig, writeReturn sig]]

writeArgs :: FunctionSignature -> Text
writeArgs sig = intercalate " " $ map (\(MkValue v) -> v) $ fsArgs sig

writeReturn :: FunctionSignature -> Text
writeReturn MkFunctionSignature { fsReturn = Return n } = concat ["-> ", n]
writeReturn MkFunctionSignature { fsReturn = NoReturn } = ""

writeFunctionCall :: FunctionCall -> Text
writeFunctionCall MkFunctionCall { fcName = fcName, fcArgs = fcArgs, tmpVarName = tvn } = case tvn of
  Nothing -> intercalate " " $ fcName : fcArgs
  Just t  -> concat [t, " <- ", intercalate " " $ fcName : fcArgs]
