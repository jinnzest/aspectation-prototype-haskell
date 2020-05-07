module ML
  ( runML
  , runMLE
  ) where

import BasicPrelude (tshow)
import Control.Exception (ErrorCall, try)
import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Data.Text (pack)
import Data.Text.IO as TIO (putStr, putStrLn)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.Process (readProcessWithExitCode)

import AspectsIO (applyAspects)
import Codegen (codegen)
import Debug.Trace (trace)
import EmbeddedFuncs (registerEmbeddedFuncs)
import Errors (Error(MkError, MkMultiRangedError, MkRangedError, MkPositionedError), Errors(MkErrors), errors)
import Location
  ( MultiRanged(MkMultiRanged)
  , Range(MkRange)
  , Ranged(MkRanged)
  , from
  , mrItem
  , rItem
  , range
  , ranges
  , to
  , Positioned(MkPositioned, pItem, pos)
  )
import SemanticTree
  (tree, Function(fSignature), _semanticTree, SemanticTree, FunctionSignature, SemanticModel, FunctionHash)
import MainParser (parseDir)
import MainHashesIO (readMainHashesIO, writeMainHashesIO)
import Data.Map (toList, Map, fromList)
import HashesRemapping (mapOldToNewNames)
import Shared (showT)
import Data.Text as T (concat, pack)

runML :: String -> IO ()
runML dir = do
  TIO.putStrLn "\nRunning Aspectation Multi-Language v0.1\n"
  handleExceptions $ runExceptT (runMLE dir >> link dir) >>= handleErrors

handleExceptions f = do
  r <- try f
  case r :: Either ErrorCall () of
    Left err -> do
      TIO.putStr "Internal error: " >> TIO.putStrLn (tshow err)
      exitWith $ ExitFailure $ -1
    Right () -> TIO.putStrLn ""

runMLE :: String -> ExceptT Errors IO ()
runMLE dir = do
  let embFuncs = registerEmbeddedFuncs
  semanticModel <- parseDir dir embFuncs
  loadedHashes  <- liftIO $ readMainHashesIO dir
  let oldToNewNames = mapOldToNewNames loadedHashes semanticModel
  (directives, analytics) <- applyAspects dir oldToNewNames $ tree semanticModel
  liftIO $ writeMainHashesIO dir $ extractSignatures semanticModel
  codegen dir (embFuncs, directives, analytics, semanticModel)

extractSignatures :: SemanticModel -> Map FunctionSignature FunctionHash
extractSignatures model =
  fromList $ concatMap (\(h, funcs) -> map (\f -> (fSignature f, h)) funcs) $ toList $ _semanticTree $ tree model

handleErrors :: Either Errors () -> IO ()
handleErrors (Right a  ) = TIO.putStrLn "\nExecutable's been successfully generated."
handleErrors (Left  err) = do
  TIO.putStrLn "Errors: \n"
  mapM_ (TIO.putStrLn . mapError) (errors err)
  exitWith $ ExitFailure $ -1

mapError (MkRangedError MkRanged { rItem = rItem, range = MkRange { from, to } }) =
  T.concat [showT rItem, "\nposition: ", showT from, " - ", showT to]
mapError (MkMultiRangedError MkMultiRanged { mrItem = mrItem, ranges = ranges }) =
  T.concat [showT mrItem, "\npositions: ", showT ranges]
mapError (MkPositionedError MkPositioned { pItem = pItem, pos = pos }) =
  T.concat [showT pItem, "\nposition: ", showT pos]
mapError (MkError error) = showT error

link :: String -> ExceptT Errors IO ()
link dir = do
  liftIO $ TIO.putStrLn $ pack $ "\nLinking '" ++ dir ++ "/target/out' ..."
  (exitCode, _, strErr) <- liftIO $ readProcessWithExitCode
    "cc"
    [dir ++ "/target/out.o", "libtommath/libtommath.a", "-no-pie", "-o", dir ++ "/target/out"]
    ""
  case exitCode of
    ExitSuccess      -> return ()
    ExitFailure code -> throwError $ MkErrors
      [MkError $ pack ("Failure linking. Exit code=" ++ show code), MkError $ pack $ "Linker error message: " ++ strErr]
