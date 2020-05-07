module SyntaxParserTest
  ( syntaxParserTest
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (fromList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Location (Range(MkRange), Ranged(MkRanged), from, rItem, range, to)
import SyntaxParser (parseSyntax)
import SyntaxTree
  ( Expression(MkIdentifier, MkInteger)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName)
  , Statement(MkNoAssignment)
  , SyntaxTree
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestShared (parseStep)
import Text.Megaparsec
  ( ErrorFancy(ErrorIndentation)
  , ErrorItem(EndOfInput, Label, Tokens)
  , ParseError(FancyError, TrivialError)
  , ParseErrorBundle(ParseErrorBundle, bundleErrors, bundlePosState)
  , Tokens
  , errorBundlePretty
  , parse
  )
import Text.Megaparsec.Pos (SourcePos(SourcePos, sourceColumn, sourceLine, sourceName), mkPos)

syntaxParserTest :: TestTree
syntaxParserTest = testGroup
  "mains syntax parser test"
  [ testCase "no input" $ do
    result <- testParse ""
    let
      error = case result of
        Left ParseErrorBundle { bundleErrors = bundleErrors, bundlePosState = bundlePosState } -> Left bundleErrors
        Right rv -> Right rv
    assertEqual "errors mismatch" (Left (TrivialError 0 endOfInput (fromList [fn]) :| [])) error
  , testCase "spaces only input" $ do
    result <- testParse "     "
    let
      error = case result of
        Left ParseErrorBundle { bundleErrors = bundleErrors, bundlePosState = bundlePosState } -> Left bundleErrors
        Right rv -> Right rv
    assertEqual "errors mismatch" (Left (FancyError 5 (fromList [ErrorIndentation EQ (mkPos 1) (mkPos 6)]) :| [])) error
  , testCase "new lines only input" $ do
    result <- testParse "\n\n\n"
    let
      error = case result of
        Left ParseErrorBundle { bundleErrors = bundleErrors, bundlePosState = bundlePosState } -> Left bundleErrors
        Right rv -> Right rv
    assertEqual "errors mismatch" (Left (TrivialError 3 endOfInput (fromList [fn]) :| [])) error
  , testCase "a function misses name args and body" $ do
    result <- testParse "fn"
    let
      error = case result of
        Left ParseErrorBundle { bundleErrors = bundleErrors, bundlePosState = bundlePosState } -> Left bundleErrors
        Right rv -> Right rv
    assertEqual "errors mismatch" (Left (TrivialError 2 endOfInput (fromList [letter]) :| [])) error
  , testCase "a function misses args and body" $ do
    result <- testParse "fn="
    let
      error = case result of
        Left ParseErrorBundle { bundleErrors = bundleErrors, bundlePosState = bundlePosState } -> Left bundleErrors
        Right rv -> Right rv
    assertEqual "errors mismatch" (Left (TrivialError 2 (Just (Tokens ('=' :| ""))) (fromList [letter]) :| [])) error
  , testCase "a function misses body" $ do
    result <- testParse "fn name ="
    let
      error = case result of
        Left  error -> Left (errorBundlePretty error)
        Right rv    -> Right rv
    assertEqual
      "errors mismatch"
      (Left
        "1:10:\n  |\n1 | fn name =\n  |          ^\nunexpected end of input\nexpecting '-', end of line, integer, or letter\n"
      )
      error
  , testCase "a function with constant" $ do
    result <- testParse "fn name = 1   "
    assertEqual
      ""
      (Right
        [ ranged
            (1, 1)
            (1, 15)
            (MkFunction
              { fSignature = ranged
                (1, 1)
                (1, 9)
                MkFunctionSignature { fsName = ranged (1, 4) (1, 9) "name", fsArgs = [] }
              , fBody      = [ranged (1, 11) (1, 15) (MkNoAssignment [ranged (1, 11) (1, 15) (MkInteger 1)])]
              }
            )
        ]
      )
      result
  , testCase "a function with an identifier and a constant" $ do
    result <- testParse "fn name = print 123 "
    assertEqual
      ""
      (Right
        [ ranged
            (1, 1)
            (1, 21)
            (MkFunction
              { fSignature = ranged
                (1, 1)
                (1, 9)
                MkFunctionSignature { fsName = ranged (1, 4) (1, 9) "name", fsArgs = [] }
              , fBody      =
                [ ranged
                    (1, 11)
                    (1, 21)
                    (MkNoAssignment
                      [ranged (1, 11) (1, 16) (MkIdentifier "print"), ranged (1, 17) (1, 21) (MkInteger 123)]
                    )
                ]
              }
            )
        ]
      )
      result
  , testCase "a function with args and a body" $ do
    result <- testParse "fn  name    arg1    arg2=   arg2  "
    assertEqual
      "errors mismatch"
      (Right
        [ ranged
            (1, 1)
            (1, 33)
            (MkFunction
              { fSignature = ranged
                (1, 1)
                (1, 25)
                MkFunctionSignature
                  { fsName = ranged (1, 5) (1, 13) "name"
                  , fsArgs = [ranged (1, 13) (1, 21) "arg1", ranged (1, 21) (1, 25) "arg2"]
                  }
              , fBody      = [ranged (1, 29) (1, 33) (MkNoAssignment [ranged (1, 29) (1, 33) (MkIdentifier "arg2")])]
              }
            )
        ]
      )
      result
  ]

endOfInput = Just EndOfInput

letter = Label ('l' :| "etter")

digit = Label ('d' :| "igit")

endOfLine = Label ('e' :| "nd of line")

neg = Tokens ('-' :| "")

fn = Tokens ('f' :| "n")

ranged (fl, fc) (tl, tc) rItem =
  let sourcePos fl fc = SourcePos { sourceName = "", sourceLine = mkPos fl, sourceColumn = mkPos fc }
  in MkRanged { range = MkRange { from = sourcePos fl fc, to = sourcePos tl tc }, rItem }

testParse :: String -> IO (Either (ParseErrorBundle Text Void) SyntaxTree)
testParse src = return $ parse parseSyntax "" $ pack src
