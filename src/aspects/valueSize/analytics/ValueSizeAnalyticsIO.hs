module ValueSizeAnalyticsIO
  ( ValueSizeAnalyticsIO.generateValueSizeAnalytics
  , writeValueSizeAnalytics
  ) where

import Control.Monad.Except (ExceptT(ExceptT), MonadIO(liftIO), mapExceptT, Monad(return))
import Data.Text.IO (putStrLn, writeFile)
import Data.Functor.Identity (Identity(runIdentity))
import GHC.Types (IO(IO))
import Data.String (String)

import Errors (Errors)
import SemanticTree (SemanticTree)
import ValueSizeAnalytics as Pure
import AspectsParserIO (parseAspectsOnPath)
import ValueSizeWriters (writeValueSizeAnalyticsMap)
import Prelude ()
import Data.Function (($), (.))
import Data.List ((++))
import EmbeddedFuncs (embeddedSignatures)
import Data.Map (empty)
import ValueSizeData (ValueSizeAnalytics)
import FunctionDomainDirectives (FunctionDomainDirectives)
import Debug.Trace (trace)
import Text.Show (Show(show))
import AspectsShared (analyticsDirWith, analyticsExt)

generateValueSizeAnalytics
  :: SemanticTree -> FunctionDomainDirectives -> ValueSizeAnalytics -> ExceptT Errors IO ValueSizeAnalytics
generateValueSizeAnalytics coreTree domainDirectives loadedAnalytics = do
  liftIO $ putStrLn "Generating value size analytics..."
  return $ Pure.generateValueSizeAnalytics embeddedSignatures coreTree domainDirectives loadedAnalytics

writeValueSizeAnalytics :: String -> ValueSizeAnalytics -> SemanticTree -> IO ()
writeValueSizeAnalytics dir analytics tree = do
  liftIO $ putStrLn "Writing value size analytics..."
  let
    body = -- trace ("\n\nanalytics: "++show analytics) 
      writeValueSizeAnalyticsMap tree analytics
  let path = argumentSizeFilePath dir
  writeFile path body
  return ()

argumentSizeFilePath :: String -> String
argumentSizeFilePath = analyticsDirWith $ "value-size." ++ analyticsExt
