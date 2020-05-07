module FunctionDomainAnalyticsIO
  ( FunctionDomainAnalytics
  , readFunctionDomainAnalytics
  , writeFunctionDomainAnalytics
  , FunctionDomainAnalyticsIO.generateFunctionDomainAnalytics
  ) where

import Prelude ()
import System.IO (IO)
import Data.String (String)
import Data.List ((++))
import Data.Function (($), (.))
import Control.Monad (when, Monad(return))
import Control.Monad.Except (ExceptT, Except, liftIO, throwError, mapExceptT)
import Data.Map as M (Map, empty, fromList, lookup, toList, member, insert)
import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust, isJust)
import Data.Sort (sortBy)
import Data.Text (Text, append, pack)
import Data.List as L (concat, last, filter, find, head, (!!))
import Data.List.Index (indexed)
import Data.Text.IO (putStrLn, writeFile)
import Debug.Trace (trace)
import Data.Foldable (sequence_)
import Data.Either (Either(Left, Right), isLeft, isRight, rights)
import Data.Set as S (Set, difference, fromList, toList, insert)
import Text.Megaparsec (SourcePos(SourcePos), unPos, sourceLine)
import Data.Functor.Identity (Identity, runIdentity)

import EmbeddedFuncs (embeddedSignatures)
import Location as Loc (Ranged(MkRanged), rItem, range, from)
import AspectsData as Asp (Analytics)
import AspectsParserIO (parseAspectsOnPath)
import AspectsShared (analyticsDirWith, analyticsExt)
import AspectWriters (writeFunctionSignature)
import Errors (Errors(MkErrors), Error(MkError))
import FunctionDomainDirectives (FunctionDomainDirectives)
import FunctionDomainDirectivesData as Asp
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , from
  , to
  , input
  , output
  )
import FunctionDomainAnalytics as Pure (generateFunctionDomainAnalytics)
import ParserWrapperIO (parsePath)
import SemanticTree (Function(MkFunction, fBody, fSignature), NameArgs(naArgs))
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , Return(NoReturn, Return)
  , SemanticTree(_semanticTree)
  , Statement(MkNoAssignment, expression)
  , Function
  , fsArgs
  , fBody
  , fSignature
  )
import FunctionDomainAnalyticsData
  ( FunctionDomainAnalytics(MkFunctionDomainAnalytics, _constraints, _undefinedProbabilities)
  , UndefinedProbabilityAnalytics
  )
import FunctionDomainAnalyticsParser (functionDomainAnalyticsParser)
import FunctionDomainAnalyticsWriters (writeFunctionDomainAnalyticsMap)

readFunctionDomainAnalytics :: String -> ExceptT Errors IO UndefinedProbabilityAnalytics
readFunctionDomainAnalytics dir = do
  let path    = undefinedProbabilityFilePath dir
  let message = "Parsing function domain dir..."
  parseAspectsOnPath path message functionDomainAnalyticsParser

generateFunctionDomainAnalytics
  :: SemanticTree
  -> FunctionDomainDirectives
  -> UndefinedProbabilityAnalytics
  -> ExceptT Errors IO UndefinedProbabilityAnalytics
generateFunctionDomainAnalytics coreTree directives loadedUndefinedProbabilities = do
  liftIO $ putStrLn "Generating function domain analytics..."
  return $ Pure.generateFunctionDomainAnalytics embeddedSignatures coreTree directives loadedUndefinedProbabilities

writeFunctionDomainAnalytics :: String -> SemanticTree -> UndefinedProbabilityAnalytics -> IO ()
writeFunctionDomainAnalytics dir tree analytics = do
  liftIO $ putStrLn "Writing function domain analytics..."
  let upBody = writeFunctionDomainAnalyticsMap tree analytics
  let upPath = undefinedProbabilityFilePath dir
  writeFile upPath upBody
  return ()

undefinedProbabilityFilePath :: String -> String
undefinedProbabilityFilePath = analyticsDirWith $ "undefined-probability." ++ analyticsExt
