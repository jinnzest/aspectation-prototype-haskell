module MainHashes
  ( writeMainHashes
  , parseHashesSyntax
  ) where

import Control.Monad.Except (void)
import Text.Megaparsec.Debug (dbg)
import Data.Bifunctor (bimap, first)
import Data.List (sortBy, sort)
import Data.Text as T (Text, append, concat, intercalate, null, pack)
import Data.Text.IO as TIO (putStrLn, readFile, writeFile)
import Data.Void (Void)
import Location (rItem, Ranged)
import Prelude as P
  (Bool(False, True), Either(Left, Right), String, ($), (++), (.), (<*), (==), const, foldl, map, null, return, error)

import SemanticTree as Sem
  ( Arg(MkValue)
  , FunctionSignature
  , FunctionSignature(MkFunctionSignature)
  , NameArgs(MkNameArgs)
  , FunctionHash(MkFunctionHash, statementHashes, functionHash)
  , fsArgs
  , fsName
  , fsReturn
  , naArgs
  , naName
  , Return(NoReturn, Return)
  , StatementHashes
  , ExpressionHash(expressionHash, MkExpressionHash, semanticPlace)
  , SemanticPlace(MkSemanticPlace, statementPlace, expressionPlace)
  )
import SyntaxParser
  ( Parser
  , equals
  , functionArgs
  , functionName
  , parseSkipNewLine
  , keyword
  , lexemeSc
  , identifier
  , ranged
  , returnName
  , isAbout
  )
import SyntaxToSemantic (synSignatureToNameArgs)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.Megaparsec (Parsec, eof, getSourcePos, parse, some, MonadParsec(try))
import Text.Megaparsec.Char (alphaNumChar, string, char)
import Text.Megaparsec as Text.Megaparsec.Internal ()
import Data.Map.Internal as Data.Functor.Identity ()
import SemanticTree ()
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Control.Applicative (optional, Alternative(many))
import Data.Map (Map, toList, fromList, empty)
import Text.Megaparsec.Char.Lexer (nonIndented, indentBlock, IndentOpt(IndentSome))
import Data.List.Index (indexed)
import CorePrelude (Num((+)))

writeMainHashes :: Map Sem.FunctionSignature FunctionHash -> Text
writeMainHashes hashesSignatures =
  let
    hashFuncNamePairs =
      P.map (\(s, h) -> (signatureToStr s, concat [functionHash h, writeStatementHashes $statementHashes h]))
        $ toList hashesSignatures
  in intercalate "\n" $ sort $ map (\(n, h) -> concat [n, " ~ ", h]) hashFuncNamePairs

writeStatementHashes :: StatementHashes -> Text
writeStatementHashes statements =
  concat $ map (\s -> concat ["\n\t", intercalate ", " $ map writeExpressionHash s]) statements

writeExpressionHash :: ExpressionHash -> Text
writeExpressionHash = expressionHash

parseHashesSyntax :: String -> Text -> Map Sem.FunctionSignature FunctionHash
parseHashesSyntax filePath fileBody = case parse signatureHashes filePath fileBody of
  Left  bundle -> empty
  Right res    -> fromList res

signatureHashes :: Parser [(Sem.FunctionSignature, FunctionHash)]
signatureHashes = some signatureHash <* eof

signatureHash :: Parser (Sem.FunctionSignature, FunctionHash)
signatureHash = do
  (name, args, retName, functionHash, statementHashTexts) <-
    nonIndented parseSkipNewLine $ indentBlock parseSkipNewLine $ do
      name    <- functionName
      args    <- functionArgs
      retName <- returnName
      isAbout
      hash <- parseHash
      return $ IndentSome Nothing (return . (name, args, retName, hash, )) parseStatementHash
  let
    statementHashes =
      map
          (\(si, texts) ->
            map
                (\(ei, t) -> MkExpressionHash
                  { expressionHash = t
                  , semanticPlace  = MkSemanticPlace { statementPlace = si + 1, expressionPlace = ei + 1 }
                  }
                )
              $ indexed texts
          )
        $ indexed statementHashTexts
  let hash = MkFunctionHash { functionHash, statementHashes }
  return
    ( Sem.MkFunctionSignature
      { Sem.fsName   = rItem name
      , Sem.fsArgs   = map (MkValue . rItem) args
      , Sem.fsReturn = maybe NoReturn Return retName
      }
    , hash
    )

parseHash :: Parser Text
parseHash = do
  hash <- some alphaNumChar
  return $ pack hash

parseStatementHash :: Parser [Text]
parseStatementHash = do
  hashesHead <- parseHash
  hashesTail <- many $ do
    void $ lexemeSc $ char ','
    parseHash
  void parseSkipNewLine
  return $ hashesHead : hashesTail

signatureToStr sig =
  let nameArgs = append (Sem.fsName sig) (P.foldl (\acc v -> append acc " v") "" (Sem.fsArgs sig))
  in
    case Sem.fsReturn sig of
      NoReturn    -> nameArgs
      Return name -> concat [nameArgs, " -> ", name]
