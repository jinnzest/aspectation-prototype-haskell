module SemanticParserIO
  ( parseSemanticIO
  ) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Data.Functor.Identity (runIdentity)
import EmbeddedFuncs (EmbeddedFuncs)
import Errors (Errors)
import SemanticParser (parseSemantic)
import SemanticTree (SemanticModel)
import SyntaxTree (SyntaxTree)

parseSemanticIO :: EmbeddedFuncs -> SyntaxTree -> ExceptT Errors IO SemanticModel
parseSemanticIO embeddedFuncs tree = mapExceptT (return . runIdentity) (parseSemantic embeddedFuncs tree)
