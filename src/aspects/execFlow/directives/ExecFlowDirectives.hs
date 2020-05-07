module ExecFlowDirectives
  ( defaultExecFlowDirective
  , embeddedExecFlowDirectives
  ) where
import ExecFlowDirectivesData (ExecFlowDirectives, Visibility(Priv, Pub))
import SemanticTree
  ( SemanticTree
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Function(fBody, fSignature, MkFunction)
  , Arg(MkValue)
  , Return(NoReturn)
  )
import Control.Monad.Except (Except)
import Errors (Errors)
import Data.Text (Text)
import Data.Map as M (fromList)

defaultExecFlowDirective :: Function -> Visibility
defaultExecFlowDirective MkFunction { fSignature = MkFunctionSignature { fsName = name } } =
  if name == "main" then Pub else Priv

embeddedExecFlowDirectives :: ExecFlowDirectives
embeddedExecFlowDirectives =
  M.fromList [(MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn }, Priv)]
