module ExecFlowDirectivesIO
  ( readExecFlowDirectives
  , writeExecFlowDirectivesWithDefaults
  ) where

import Prelude ()
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, mapExceptT, MonadIO(liftIO))

import AspectsParserIO (parseAspectsOnPath)
import Errors (Errors)
import ExecFlowDirectivesData (ExecFlowDirectives, ExecFlowDirectives)
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
import ExecFlowDirectivesParser (execFlowDirectivesParser)
import ExecFLowDirectivesWriter (writeExecFlowDirectives)
import ExecFlowDirectives (embeddedExecFlowDirectives)
import Data.Map (union)

readExecFlowDirectives :: String -> ExceptT Errors IO ExecFlowDirectives
readExecFlowDirectives dir = do
  let path    = execFlowDirectivesFilePath dir
  let message = "Parsing execution flow directives..."
  efDirectives <- parseAspectsOnPath path message execFlowDirectivesParser
  let
    res =
      ((embeddedExecFlowDirectives :: ExecFlowDirectives) `union` (efDirectives :: ExecFlowDirectives)) :: ExecFlowDirectives
  return res

writeExecFlowDirectivesWithDefaults :: String -> ExecFlowDirectives -> IO ()
writeExecFlowDirectivesWithDefaults dir directives = do
  liftIO $ putStrLn "Writing execution flow directives with defaults..."
  let body = writeExecFlowDirectives directives
  let path = execFlowDirectivesWithDefaultsFilePath dir
  writeFile path body
  return ()

execFlowDirectivesFilePath :: String -> String
execFlowDirectivesFilePath = directivesDirWith ("exec-flow." ++ directivesExt)
execFlowDirectivesWithDefaultsFilePath :: String -> String
execFlowDirectivesWithDefaultsFilePath = directivesWithDefaultsDirWith $ "exec-flow." ++ directivesExt
