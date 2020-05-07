module SyntaxToSemantic
  ( stsSignature
  , synSignatureToNameArgs
  , semSignatureToNameArgs
  ) where

import Location (rItem)
import Prelude (Bool(False), ($), (.), const, map)
import SemanticTree as Sem
  ( Arg(MkValue)
  , FunctionSignature(MkFunctionSignature)
  , NameArgs(MkNameArgs)
  , Return(NoReturn)
  , fsArgs
  , fsName
  , fsReturn
  , naArgs
  , naName
  )
import SyntaxTree as Syn (FunctionSignature(fsName, fsArgs))

stsSignature :: Syn.FunctionSignature -> Sem.FunctionSignature
stsSignature sig = Sem.MkFunctionSignature
  { Sem.fsName   = rItem $ Syn.fsName sig
  , Sem.fsArgs   = map (MkValue . rItem) $ Syn.fsArgs sig
  , Sem.fsReturn = NoReturn
  }

synSignatureToNameArgs :: Syn.FunctionSignature -> NameArgs
synSignatureToNameArgs sig =
  Sem.MkNameArgs { Sem.naName = rItem $ Syn.fsName sig, Sem.naArgs = map (MkValue . rItem) $ Syn.fsArgs sig }

semSignatureToNameArgs :: Sem.FunctionSignature -> NameArgs
semSignatureToNameArgs sig = Sem.MkNameArgs { Sem.naName = Sem.fsName sig, Sem.naArgs = Sem.fsArgs sig }
