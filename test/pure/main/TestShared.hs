module TestShared
  ( Parser
  , parseStep
  , testParse
  , testParseError
  , setOf
  , mkRange
  ) where

import Control.Monad.Except (runExcept)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Panic (panic)
import Prelude as P (Either(Left, Right), Ordering(EQ), String, ($), (++), (==), compare, null, return, show)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Error (ParseError(TrivialError), errorBundlePretty)

import EmbeddedFuncs (EmbeddedFuncs(MkEmbeddedFuncs), printF)
import Errors (Error(MkError), Errors(MkErrors, errors))
import SemanticParser (parseSemantic)
import SemanticTree
  ( Arg(MkValue)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , Return(NoReturn)
  , SemanticModel(MkSemanticModel)
  , SemanticTree
  )
import SyntaxParser (parseSyntax)
import Debug.Trace (trace)
import Data.Set as S (Set, fromList)
import Test.Tasty.QuickCheck (Gen, vectorOf)
import Data.Int (Int)
import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Megaparsec.Pos (SourcePos(SourcePos), mkPos, sourceColumn, sourceLine, sourceName)
import Location
  ( MultiRanged(MkMultiRanged)
  , Positioned(MkPositioned)
  , Range(MkRange)
  , Ranged(MkRanged)
  , from
  , mrItem
  , rItem
  , range
  , ranges
  , to
  )

type Parser = Parsec Void Text

parseStep :: Text -> String
parseStep src = "parse '" ++ unpack src ++ "'"

testParse :: Text -> SemanticTree
testParse input =
  let
    result = runExcept $ do
      tree <- case parse parseSyntax "" input of
        Right t -> return t
        Left  l -> panic $ errorBundlePretty l
      parseSemantic
        MkEmbeddedFuncs
          { printF = MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn }
          }
        tree
  in
    case result of
      Right (MkSemanticModel tree) -> tree
      Left  unexpected             -> panic ("expected a tree instead got: " ++ show unexpected)

testParseError :: Text -> [Error]
testParseError input =
  let
    result = runExcept $ do
      tree <- case parse parseSyntax "" input of
        Right t -> return t
        Left  l -> panic $ show l
      parseSemantic
        MkEmbeddedFuncs
          { printF = MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn }
          }
        tree
  in
    case result of
      (Left  MkErrors { errors = errors }) -> errors
      (Right unexpected                  ) -> panic ("expected an error instead got: " ++ show unexpected)

setOf :: (Eq a, Ord a) => Int -> Gen a -> Gen (S.Set a)
setOf size arb = do
  res <- vectorOf size arb
  return $ S.fromList res

mkRange (lf, cf) (lt, ct) =
  let sourcePos l c = SourcePos { sourceName = "", sourceLine = mkPos l, sourceColumn = mkPos c }
  in MkRange { from = sourcePos lf cf, to = sourcePos lt ct }
