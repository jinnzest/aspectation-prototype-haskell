module ExecFlowAnalyticsParser
  ( execFlowAnalyticsParser
  ) where

import Prelude ()
import SyntaxParser (Parser, isAbout, keyword)
import Text.Megaparsec (many, MonadParsec(eof), getSourcePos, optional, some)
import Location as L (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import Text.Megaparsec.Char.Lexer (IndentOpt(IndentMany), nonIndented, indentBlock)
import AspectsParser (parseSemanticFunction, parseSkipNewLine)
import Control.Monad (Monad(return))
import Data.Function (($), (.))
import Control.Applicative ((<$>), (<*))
import Data.Maybe (Maybe(Nothing), fromMaybe)
import SemanticTree (FunctionSignature)

execFlowAnalyticsParser :: Parser [Ranged (FunctionSignature, [[FunctionSignature]])]
execFlowAnalyticsParser = many execFlowAnalyticsItem <* eof

execFlowAnalyticsItem :: Parser (Ranged (FunctionSignature, [[FunctionSignature]]))
execFlowAnalyticsItem = do
  from             <- getSourcePos
  (sig, funcCalls) <- nonIndented parseSkipNewLine $ indentBlock parseSkipNewLine $ do
    sig <- parseSemanticFunction
    isAbout
    return $ IndentMany Nothing (return . (sig, )) parseExecFlowCalls
  to <- getSourcePos
  return $ --trace ("\nVAL: "++show val) $
           MkRanged { rItem = (sig, funcCalls), range = MkRange { L.from, L.to } }

parseExecFlowCalls :: Parser [FunctionSignature]
parseExecFlowCalls = do
  head <- parseSemanticFunction
  tail <- many parseExecFlowCall
  return $ head : tail

parseExecFlowCall :: Parser FunctionSignature
parseExecFlowCall = do
  keyword ","
  parseSemanticFunction
