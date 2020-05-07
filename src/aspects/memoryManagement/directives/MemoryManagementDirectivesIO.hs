module MemoryManagementDirectivesIO
  ( MemoryManagementDirectives
  , readMemoryManagementDirectives
  , writeMemoryManagementDirectivesWithDefaults
  ) where

import Control.Monad.Except (ExceptT, liftIO, mapExceptT)
import Data.Map as M (Map, empty, fromList, toList)
import Data.Text (Text)
import Data.Text.IO (putStrLn, writeFile)
import Prelude (IO, Maybe, String, ($), return)

import AspectsData (Directives)
import Errors (Errors)
import SemanticTree
  ( Arg(MkValue)
  , Return(NoReturn, Return)
  , SemanticTree
  , Function(MkFunction, fSignature, fBody)
  , Statement(MkNoAssignment, expression)
  , Expression(MkConstantInteger, fcsName, fcsArgs)
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Expression(MkFunctionCall)
  )
import MemoryManagementDirectivesData (MemoryManagementDirectives, MemoryManagementDirective(AllocHere))
import Data.Maybe (Maybe(Nothing, Just))
import MemoryManagementDirectiveWriters (writeMemoryManagementDirectivesMap)
import AspectsShared (directivesWithDefaultsDirWith, directivesDirWith, directivesExt)
import Data.List as L ((++), map)
import Data.List.Index (indexed)
import ValueSizeData (ValueSizeAnalytics)
import Location (Ranged(rItem, MkRanged, range))
import AspectsParserIO (parseAspectsOnPath)
import MemoryManagementDirectivesParser (resourcesManagementDirectives)
import Data.Function (const, (.))
import qualified MemoryManagementDirectives as Pure
import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Set as S

resourcesManagementDirectivesFilePath :: String -> String
resourcesManagementDirectivesFilePath = directivesDirWith ("resources_management." ++ directivesExt)

readMemoryManagementDirectives :: String -> ExceptT Errors IO MemoryManagementDirectives
readMemoryManagementDirectives dir =
  let
    path    = resourcesManagementDirectivesFilePath dir
    message = "Parsing function domain dir..."
  in parseAspectsOnPath path message resourcesManagementDirectives

writeMemoryManagementDirectivesWithDefaults :: String -> SemanticTree -> MemoryManagementDirectives -> IO ()
writeMemoryManagementDirectivesWithDefaults dir tree directives = do
  liftIO $ putStrLn "Writing function domain directives with defaults..."
  let body = writeMemoryManagementDirectivesMap tree directives
  let path = resourcesManagementDirectivesWithDefaultsFilePath dir
  writeFile path body
  return ()

resourcesManagementDirectivesWithDefaultsFilePath :: String -> String
resourcesManagementDirectivesWithDefaultsFilePath = directivesWithDefaultsDirWith "memory-management.astnd"
