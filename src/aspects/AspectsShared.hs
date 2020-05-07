module AspectsShared
  ( generatedDirWith
  , analyticsDirWith
  , directivesDirWith
  , directivesDir
  , directivesWithDefaultsDirWith
  , directivesWithDefaultsDir
  , analyticsDir
  , applyAspect
  , analyticsExt
  , directivesExt
  , createAbsentFile
  , writeStatement
  , extractFunctions
  , extractLine
  , writeExpression
  ) where

import Prelude ()
import AspectsData as Asp (Directives, Analytics)
import SemanticTree as Sem
  ( Arg(MkValue)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , Return(NoReturn, Return)
  , fBody
  , fSignature
  , SemanticTree(_semanticTree)
  , FunctionHash
  , Statement(MkNoAssignment, expression)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsName, fcsArgs, tmpVarName)
  , showArg
  )
import Control.Monad.State.Lazy (MonadIO(liftIO), unless)
import Data.Text as T (Text, concat, pack, intercalate)
import Data.Text.IO as TIO (putStrLn)
import Shared (srcDir, showT)
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(ReadWriteMode), hClose, IO)
import Data.Map (Map)
import HashesRemapping (remap)
import Data.String (String)
import Data.Function (($), (.))
import Control.Monad (Monad(return))
import Data.List (map, (++), (!!), foldl)
import Data.Maybe (Maybe(Nothing), Maybe(Just))
import Debug.Trace (trace)
import Text.Show (Show(show))
import Location (Ranged(MkRanged, rItem, range))
import Data.List as L (length)
import Data.Bool (Bool(True, False))
import Panic (panic)
import Data.Ord (Ord((>=)))
import Data.Map as M (Map, insert, empty, toList)
import AspectsRegister (AnalyticsRegister)
import Location as Loc (Range(from))
import CorePrelude (Integral(toInteger), Integer, error)
import Text.Megaparsec.Pos (unPos, SourcePos(sourceLine))

generatedDirWith :: String -> String -> String
generatedDirWith path dir = dir ++ "/generated/" ++ path
generatedDir = generatedDirWith ""
directivesDirWith :: String -> String -> String
directivesDirWith path = generatedDirWith $ "directives/" ++ path
directivesDir = directivesDirWith ""
analyticsDirWith :: String -> String -> String
analyticsDirWith path = generatedDirWith $ "analytics/" ++ path
analyticsDir = analyticsDirWith ""
directivesWithDefaultsDirWith :: String -> String -> String
directivesWithDefaultsDirWith path = generatedDirWith $ "directives-with-defaults/" ++ path
directivesWithDefaultsDir = directivesWithDefaultsDirWith ""
analyticsExt = "astna"
directivesExt = "astnd"

createAbsentFile :: String -> IO ()
createAbsentFile filePath = do
  exists <- doesFileExist filePath
  unless exists $ do
    handle <- openFile filePath ReadWriteMode
    hClose handle
    return ()

applyAspect
  :: MonadIO m
  => Text
  -> String
  -> SemanticTree
  -> aspects
  -> Map Sem.FunctionSignature [Sem.FunctionSignature]
  -> (String -> m (Directives d))
  -> (String -> Directives d -> SemanticTree -> IO ())
  -> (Directives d -> SemanticTree -> m (Directives d))
  -> (aspects -> Directives d -> SemanticTree -> Directives d)
  -> (String -> m (Analytics a))
  -> (String -> Analytics a -> SemanticTree -> IO ())
  -> (aspects -> Analytics a -> Directives d -> SemanticTree -> m (Analytics a))
  -> m (Directives d, Analytics a)
applyAspect name dir coreTree aspects oldToNewNames directivesReader directivesWithDefaultsWriter directivesPropagator defaultDirectivesGenerator analyticsReader analyticsWriter analyticsGenerator
  = do
    liftIO $ TIO.putStrLn $ T.concat ["\nApplying aspect '", name, "' ..."]
    loadedDirectives <- directivesReader dir
    let remappedDirectives = remap loadedDirectives oldToNewNames
    propagatedDirectives <- directivesPropagator remappedDirectives coreTree
    let directivesWithDefaults = defaultDirectivesGenerator aspects propagatedDirectives coreTree
    liftIO $ directivesWithDefaultsWriter dir directivesWithDefaults coreTree
    loadedAnalytics <- analyticsReader dir
    let remappedAnalytics = remap loadedAnalytics oldToNewNames
    generatedAnalytics <- analyticsGenerator aspects remappedAnalytics directivesWithDefaults coreTree
    liftIO $ analyticsWriter dir generatedAnalytics coreTree
    return (directivesWithDefaults, generatedAnalytics)

writeStatement :: (FunctionSignature, Ranged Statement) -> Text
writeStatement (sig, MkRanged { rItem = MkNoAssignment { Sem.expression = expr } }) = writeExpression (sig, expr)

writeExpression (_, MkConstantInteger _ MkRanged { rItem = v }) = showT v
writeExpression (MkFunctionSignature { fsArgs = args }, MkFunctionArgument _ MkRanged { rItem = v }) =
  if v >= L.length args then panic ("\nargs: " ++ show args ++ "\nv: " ++ show v) else unpackArg $ args !! v
writeExpression (sig, Sem.MkFunctionCall { fcsName = name, fcsArgs = args, tmpVarName = tvn }) =
  intercalate " "
    $  (case tvn of
         Nothing -> []
         Just r  -> [r, "<-"]
       )
    ++ (rItem name : map (\a -> writeExpression (sig, a)) args)

unpackArg :: Arg -> Text
unpackArg (MkValue v) = v

isFuncCall :: Expression -> Bool
isFuncCall Sem.MkFunctionCall{} = True
isFuncCall _                    = False

extractFunctions :: SemanticTree -> M.Map Sem.FunctionSignature [Ranged Statement]
extractFunctions tree =
  let
    res =
      foldl (\acc (h, functions) -> foldl (\acc2 f -> M.insert (fSignature f) (fBody f) acc2) acc functions) M.empty
        $ M.toList
        $ _semanticTree tree
  in --trace("\n\n\nextractFunctions IN: "++show tree++"\nextractFunctions OUT: "++show res) 
     res

extractLine :: Loc.Range -> Integer
extractLine = toInteger . unPos . sourceLine . Loc.from
