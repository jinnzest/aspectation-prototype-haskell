module FunctionDomainDirectivesGeneratorTest
  ( functionDomainDirectivesGeneratorTest
  ) where

import Prelude ()
import SemanticTreeArbitrary ()
import Data.Int (Int)
import Control.Monad.Except (Except, runExceptT, mapExceptT)
import Data.Map as M (Map, empty, fromList, lookup, member, toList)
import Data.List as L (sortBy, maximumBy, null, map, filter, length, zipWith, zip)
import Data.Text (Text, append, pack, unpack)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, choose, oneof, testProperty)
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Pos (SourcePos(SourcePos), mkPos, sourceColumn, sourceLine, sourceName)
import Data.Either (Either(Right), Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Control.Monad (Monad(return))
import Data.Function ((.), ($))
import Data.List ((++))
import Text.Show (Show(show))
import Data.Functor ((<$>))
import Data.Bool (not, Bool(True))
import Data.Ord (Ord(compare))
import Data.Tuple (fst)
import GHC.Num (Num((+)))
import Data.Eq (Eq((/=)), Eq((==)))
import Control.Monad.Identity (Identity(runIdentity))
import Test.Tasty.HUnit (testCase, assertEqual)
import AspectsData as Asp (Analytics, Directives)
import Errors (Error(MkError), Errors(MkErrors))
import FunctionDomainAnalyticsIO (readFunctionDomainAnalytics, writeFunctionDomainAnalytics)
import FunctionDomainDirectives (propagateFunctionDomainDirectives)
import FunctionDomainDirectivesData as FDD
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainDirectives
  , from
  , input
  , output
  , to
  )
import FunctionDomainDirectivesParser (functionDomainDirectivesParser)
import FunctionDomainDirectivesWriters (writeFunctionDomainDirectivesMap)
import Location as Loc (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import Panic (panic)
import ParserWrapper (parse)
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , NameArgs(MkNameArgs, naArgs, naName)
  , Return(NoReturn, Return)
  , Return(NoReturn, Return)
  , FunctionHash(MkFunctionHash)
  , SemanticModel(MkSemanticModel)
  , SemanticTree(MkSemanticTree)
  , Statement(MkNoAssignment)
  , fsArgs
  , fsName
  , fsReturn
  )
import ShowJ (showJ)
import TestShared (parseStep, testParse)
import FunctionDomainTestShared (testParseFunctionDomainDirectives)

functionDomainDirectivesGeneratorTest :: TestTree
functionDomainDirectivesGeneratorTest = testGroup
  "Function domain directives generator tests"
  [ testProperty "propagate nothing when no directives defined" $ \tree ->
    let
      initialDirectives = M.empty
      result            = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
      result2           = result
    in result2 == initialDirectives
  , testCase "test lookup" $ do
    let mp = M.fromList [(MkValue "b", True)]
    assertEqual "" (Just True) (M.lookup (MkValue "b") mp)
  , testCase "propagate a function by args without nested functions" $ do
    let initialDirectives = testParseFunctionDomainDirectives "func arg ~  -5<=arg<=10\nprint v  ~  -inf<=v<=inf"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives "main arg ~  -5<=arg<=10\nprint v ~  -inf<=v<=inf\nfunc arg ~  -5<=arg<=10"
    let tree                 = testParse "fn main arg = func arg\nfn func arg = print arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate a function by arg return without nested functions" $ do
    let initialDirectives = testParseFunctionDomainDirectives "main arg -> r ~  -5<=arg<=10"
    let expectedDirectives = testParseFunctionDomainDirectives "main arg -> r ~ -5<=arg<=10, -5<=r<=10"
    let tree                 = testParse "fn main arg = arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate a function by the first argument passed through the second argument of child function" $ do
    let initialDirectives = testParseFunctionDomainDirectives "func a b c~ -5<=a<=10, 20<=c<=30\nprint x~-inf<=x<=inf"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives "main v~20<=v<=30\nprint x~-inf<=x<=inf\nfunc a b c ~ -5<=a<=10, 20<=c<=30"
    let tree                 = testParse "fn main arg = func 1 2 arg\nfn func arg1 arg2 arg3 = print arg3"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate two functions on the same level" $ do
    let
      initialDirectives =
        testParseFunctionDomainDirectives "func1 v ~  -10<=v<=5\nprint v ~  -inf<=v<=inf\nfunc2 v ~  -5<=v<=10"
    let
      expectedDirectives = testParseFunctionDomainDirectives
        "main v ~  -5<=v<=5\nfunc1 v ~  -10<=v<=5\nprint v ~  -inf<=v<=inf\nfunc2 v ~  -5<=v<=10"
    let tree = testParse "fn main arg = \n\tfunc1 arg\n\tfunc2 arg\nfn func1 arg = print arg\nfn func2 arg = print arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate two functions on different levels" $ do
    let
      initialDirectives =
        testParseFunctionDomainDirectives "func2 v ~  -5<=v<=10\nprint v ~  -inf<=v<=inf\nfunc3 v ~  -10<=v<=5"
    let
      expectedDirectives = testParseFunctionDomainDirectives
        "func3 v ~  -10<=v<=5\nmain v ~ -5<=v<=5\nfunc1 v ~  -5<=v<=5\nprint v ~  -inf<=v<=inf\nfunc2 v ~  -5<=v<=10"
    let
      tree =
        testParse
          "fn main arg = func1 arg\nfn func1 arg =\n\tfunc2 arg\n\tfunc3 arg\nfn func2 arg = print arg\nfn func3 arg = print arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate a funtion through two functions on the same level" $ do
    let
      initialDirectives =
        testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc3 arg1 arg2 ~  -10<=arg1<=5, -5<=arg2<=10"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives
          "func3 arg1 arg2 ~  -10<=arg1<=5,-5<=arg2<=10\nmain arg ~  -5<=arg<=5\nfunc1 arg ~  -5<=arg<=10\nprint v ~  -inf<=v<=inf\nfunc2 arg ~ -10<=arg<=5"
    let
      tree =
        testParse
          "fn main arg =\n\tfunc1 arg\n\tfunc2 arg\nfn func1 arg = func3 1 arg\nfn func2 arg = func3 arg 2\nfn func3 arg1 arg2 = print arg1"
    let
      propagatedDirectives =
        -- trace ("\ninitialDirectives: " ++ show initialDirectives) 
        extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate one function having result of another one as argument" $ do
    let initialDirectives = testParseFunctionDomainDirectives "func arg -> r ~  -5<=arg<=10\nprint v ~  -inf<=v<=inf"
    let tree              = testParse "fn main arg = print func arg\nfn func arg = arg"
    let
      expectedDirectives = testParseFunctionDomainDirectives
        "main v ~  -5<=v<=10\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -5<=arg<=10, -5<=r<=10"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    -- trace
    --   (  "\n\ntree: "
    --   ++ show tree
    --   ++ "\ninitialDirectives:    "
    --   ++ show initialDirectives
    --   ++ "\nexpectedDirectives:   "
    --   ++ show expectedDirectives
    --   ++ "\npropagatedDirectives: "
    --   ++ show propagatedDirectives
    --   )
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "propagate a funtion through two functions on the same level with multi domain ranges" $ do
    let
      initialDirectives = testParseFunctionDomainDirectives
        "print v ~ -inf<=v<=inf\nfunc3 a b ~  -10<=a<=5 or 20<=a<=40, -5<=b<=10 or 15<=b<=50"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives
          "func3 a b ~  -10<=a<=5 or 20<=a<=40, -5<=b<=10 or 15<=b<=50\nmain v ~  -5<=v<=5 or 20<=v<=40\nfunc1 v ~   -5<=v<=10 or 15<=v<=50\nprint v ~  -inf<=v<=inf\nfunc2 v ~-10<=v<=5 or 20<=v<=40"
    let
      tree =
        testParse
          "fn main arg =\n\tfunc1 arg\n\tfunc2 arg\nfn func1 arg = func3 1 arg\nfn func2 arg = func3 arg 2\nfn func3 arg1 arg2 = print arg1"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "merge two functions domain ranges 1" $ do
    let
      initialDirectives = testParseFunctionDomainDirectives
        "print v ~ -inf<=v<=inf\nfunc1 v ~ -10<=v<=30\nfunc2 a ~  -20<=a<=5 or 20<=a<=40"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives
          "print v ~ -inf<=v<=inf\nfunc1 v ~ -10<=v<=30\nfunc2 a ~  -20<=a<=5 or 20<=a<=40\nmain a ~  -10<=a<=5 or 20<=a<=30"
    let tree = testParse "fn main arg =\n\tfunc1 arg\n\tfunc2 arg\nfn func1 arg = print arg\nfn func2 arg = print arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "merge two functions domain ranges 2" $ do
    let
      initialDirectives = testParseFunctionDomainDirectives
        "print v ~ -inf<=v<=inf\nfunc1 a ~  -20<=a<=5 or 20<=a<=40\nfunc2 a ~   -10<=a<=30"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives
          "print v ~ -inf<=v<=inf\nfunc1 a ~  -20<=a<=5 or 20<=a<=40\nfunc2 a ~   -10<=a<=30\nmain v ~  -10<=v<=5 or 20<=v<=30"
    let tree = testParse "fn main arg =\n\tfunc1 arg\n\tfunc2 arg\nfn func1 arg = print arg\nfn func2 arg = print arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "no intersection of constraints" $ do
    let
      initialDirectives =
        testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc1 a ~  -20<=a<=5\nfunc2 a ~   10<=a<=30"
    let
      expectedError = MkErrors
        [MkError "Function 'main' is always undefined for domain intersection of the input directives of sub-functions"]
    let tree = testParse "fn main arg =\n\tfunc1 arg\n\tfunc2 arg\nfn func1 arg = print arg\nfn func2 arg = print arg"
    let propagatedDirectives = extractError $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedError propagatedDirectives
  , testCase "an argument is not being used in a domain definition" $ do
    let initialDirectives = testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc1 a b ~  -inf<=a<=inf"
    let
      expectedDirectives =
        testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc1 a b ~  -inf<=a<=inf\nmain a ~  -inf<=a<=inf"
    let tree                 = testParse "fn main arg =\n\tfunc1 arg arg\nfn func1 arg arg2 = \n\tprint arg"
    let propagatedDirectives = extractDirectives $ propagateFunctionDomainDirectives initialDirectives tree
    assertEqual "" expectedDirectives propagatedDirectives
  , testCase "unknown arg is being used in a domain definition" $ do
    let error = testParseError "print v ~ -inf<=v<=inf\nfunc1 v ~  -20<=a<=5"
    let
      expectedError =
        MkErrors [MkError "2:21:\n  |\n2 | func1 v ~  -20<=a<=5\n  |                     ^\nThere is no alias 'a'\n"]
    assertEqual "" expectedError error
  , testCase "unknown return alias is being used in a domain definition" $ do
    let error = testParseError "print v ~ -inf<=v<=inf\nfunc1 -> r ~ -inf<=b<=inf"
    let
      expectedError = MkErrors
        [MkError "2:26:\n  |\n2 | func1 -> r ~ -inf<=b<=inf\n  |                          ^\nThere is no alias 'b'\n"]
    assertEqual "" expectedError error
  , testCase "no more than 1 alias inside a group is allowed" $ do
    let error = testParseError "print v ~ -inf<=v<=inf\nfunc1 a b-> r ~ -inf<=a<=inf or -inf<=b<=inf"
    let
      expectedError = MkErrors
        [ MkError
            "2:45:\n  |\n2 | func1 a b-> r ~ -inf<=a<=inf or -inf<=b<=inf\n  |                                             ^\nThere are more than 1 alias inside constraints group: a, b\n"
        ]
    assertEqual "" expectedError error
  , testCase "error undefined when no intersection of a function and subfunction domain"
    $ let
        directives = testParseFunctionDomainDirectives
          "main arg -> r ~ 5<=arg<=10\nprint v ~ -inf<=v<=inf\nfunc arg -> r ~ -10 <=arg<=-5"
        tree          = testParse "fn main arg = func arg\nfn func arg = arg"
        error         = extractError $ propagateFunctionDomainDirectives directives tree
        expectedError = MkErrors
          [ MkError
              "Function 'main' is always undefined for the input directives of itself and the input directives of sub-functions"
          ]
      in assertEqual "" expectedError error
  , testCase "error undefined when constant 100 as an argument is out of a function domain -10 to -5"
    $ let
        directives    = testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc arg -> r ~ -10 <=arg<=-5"
        tree          = testParse "fn main arg = func 100\nfn func arg = arg"
        error         = extractError $ propagateFunctionDomainDirectives directives tree
        expectedError = MkErrors [MkError "Function 'func' is always undefined for constant argument '100'"]
      in assertEqual "" expectedError error
  , testCase "error undefined when constant 100 as an argument is out of a function domain from -infinite to concrete"
    $ let
        directives    = testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc arg -> r ~ -inf <=arg<=-5"
        tree          = testParse "fn main arg = func 100\nfn func arg = arg"
        error         = extractError $ propagateFunctionDomainDirectives directives tree
        expectedError = MkErrors [MkError "Function 'func' is always undefined for constant argument '100'"]
      in assertEqual "" expectedError error
  , testCase "error undefined when constant 100 as an argument is out of a function domain from 300 to infinite"
    $ let
        directives    = testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc arg -> r ~ 300 <=arg<=inf"
        tree          = testParse "fn main arg = func 100\nfn func arg = arg"
        error         = extractError $ propagateFunctionDomainDirectives directives tree
        expectedError = MkErrors [MkError "Function 'func' is always undefined for constant argument '100'"]
      in assertEqual "" expectedError error
  , testCase "error undefined when a constant as a return is out of a function domain from -infinite to concrete"
    $ let
        directives =
          testParseFunctionDomainDirectives "print v ~ -inf<=v<=inf\nfunc arg -> r ~ -inf <=r<=-5, -inf<=arg<=inf"
        tree          = testParse "fn main arg = func arg\nfn func arg = 10"
        error         = extractError $ propagateFunctionDomainDirectives directives tree
        expectedError = MkErrors [MkError "Function 'func' is always undefined when returning constant '10'"]
      in assertEqual "" expectedError error
  , testCase
      "error undefined when intersection of a return from a parent function and a child domains is an empty domain"
    $ let
        directives = testParseFunctionDomainDirectives
          "parent arg -> r ~ 5<=r<=10, -inf<=arg<=inf\nchild arg -> r ~ -5 <=r<=1, -inf<=arg<=inf"
        tree          = testParse "fn main arg = parent arg\nfn parent arg = child arg\nfn child arg = 1"
        error         = extractError $ propagateFunctionDomainDirectives directives tree
        expectedError = MkErrors [MkError "Function 'parent' is always undefined when calling sub function 'child'"]
      in assertEqual "" expectedError error
  ]

maxArgsLength inputConstraints =
  if null inputConstraints then 0 else fst $ maximumBy (\(l, _) (r, _) -> compare l r) inputConstraints

maxItemsSize = 10

testParseError :: Text -> Errors
testParseError body =
  let
    parsed = parse "" body functionDomainDirectivesParser
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, FunctionDomainConstraint)])
  in case eitherResult of
    Right (Left err) -> err
    unexpected       -> panic ("Internal compiler error: " ++ showJ unexpected)

extractError :: Except Errors FunctionDomainDirectives -> Errors
extractError result =
  let
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) result) :: Either Errors (Either Errors FunctionDomainDirectives)
  in
    case eitherResult of
      Right res -> case res of
        Left  err -> err
        Right r   -> panic ("Internal compiler error: " ++ show r)
      Left unexpected -> panic ("Internal compiler error: " ++ show unexpected)

extractDirectives :: Except Errors FunctionDomainDirectives -> FunctionDomainDirectives
extractDirectives result =
  let
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) result) :: Either Errors (Either Errors FunctionDomainDirectives)
  in
    case eitherResult of
      Right res -> case res of
        Right r   -> r
        Left  err -> panic ("Unexpected: " ++ show err)
      Left err -> panic ("Unexpected: " ++ show err)
