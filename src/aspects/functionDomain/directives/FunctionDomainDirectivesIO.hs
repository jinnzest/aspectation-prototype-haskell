module FunctionDomainDirectivesIO
  ( readFunctionDomainDirectives
  , writeFunctionDomainDirectivesWithDefaults
  , FunctionDomainDirectivesIO.propagateFunctionDomainDirectives
  ) where

import Prelude ()
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, mapExceptT, MonadIO(liftIO))

import AspectsParserIO (parseAspectsOnPath)
import Errors (Errors)
import FunctionDomainDirectives as Pure (propagateFunctionDomainDirectives)
import FunctionDomainDirectivesData (FunctionDomainDirectives, FunctionDomainDirectives)
import SemanticTree (SemanticTree)
import Control.Lens.Combinators (Identity(runIdentity))
import AspectsShared (directivesDirWith, directivesExt, directivesWithDefaultsDirWith)
import Data.String (String)
import System.IO (IO)
import Data.Text.IO (writeFile, putStrLn)
import Data.List ((++))
import Data.Function (($), (.))
import Control.Monad (Monad(return))
import System.Directory (createDirectoryIfMissing)
import Data.Bool (Bool(True))
import FunctionDomainDirectivesWriters (writeFunctionDomainDirectivesMap)
import FunctionDomainDirectivesParser (functionDomainDirectivesParser)

readFunctionDomainDirectives :: String -> ExceptT Errors IO FunctionDomainDirectives
readFunctionDomainDirectives dir =
  let
    path    = functionDomainDirectivesFilePath dir
    message = "Parsing function domain dir..."
  in parseAspectsOnPath path message functionDomainDirectivesParser

writeFunctionDomainDirectivesWithDefaults :: String -> FunctionDomainDirectives -> IO ()
writeFunctionDomainDirectivesWithDefaults dir directives = do
  liftIO $ putStrLn "Writing function domain analytics..."
  let body = writeFunctionDomainDirectivesMap directives
  let path = functionDomainDirectivesWithDefaultsFilePath dir
  writeFile path body
  return ()

propagateFunctionDomainDirectives
  :: FunctionDomainDirectives -> SemanticTree -> ExceptT Errors IO FunctionDomainDirectives
propagateFunctionDomainDirectives directives tree =
  mapExceptT (return . runIdentity) $ Pure.propagateFunctionDomainDirectives directives tree

functionDomainDirectivesFilePath :: String -> String
functionDomainDirectivesFilePath = directivesDirWith ("function-domain." ++ directivesExt)
functionDomainDirectivesWithDefaultsFilePath :: String -> String
functionDomainDirectivesWithDefaultsFilePath = directivesWithDefaultsDirWith $ "function-domain." ++ directivesExt
