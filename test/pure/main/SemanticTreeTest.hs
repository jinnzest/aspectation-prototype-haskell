module SemanticTreeTest
  ( semanticTreeTest
  ) where

import Prelude ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import SemanticTree
  ( FunctionSignature(MkFunctionSignature)
  , fsName
  , fsArgs
  , fsReturn
  , Return(NoReturn)
  , Arg(MkValue)
  , NameArgs(MkNameArgs, naArgs, naName)
  )
import Data.Map (fromList, lookup)
import Data.Function (($))
import Data.Bool (Bool(True, False))
import Data.Maybe (Maybe(Just))

semanticTreeTest :: TestTree
semanticTreeTest = testGroup
  "semantic tree tests"
  [ testCase "compare two function signatures with the same name empty args and no return" $ do
    let first  = MkFunctionSignature { fsName = "func", fsArgs = [], fsReturn = NoReturn }
    let second = MkFunctionSignature { fsName = "func", fsArgs = [], fsReturn = NoReturn }
    assertEqual "" first second
  , testCase "compare two name args with the same name empty args and no return" $ do
    let first  = MkNameArgs { naName = "func", naArgs = [] }
    let second = MkNameArgs { naName = "func", naArgs = [] }
    assertEqual "" first second
  , testCase
      "compare two function signatures with the same name empty equal length args with different names and no return"
    $ do
        let first  = MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = NoReturn }
        let second = MkFunctionSignature { fsName = "func", fsArgs = [MkValue "c", MkValue "d"], fsReturn = NoReturn }
        assertEqual "" first second
  , testCase "find a function in a map when argument names differ" $ do
    let first = MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = NoReturn }
    let second = MkFunctionSignature { fsName = "func", fsArgs = [MkValue "c"], fsReturn = NoReturn }
    let toFound = MkFunctionSignature { fsName = "func", fsArgs = [MkValue "d"], fsReturn = NoReturn }
    let m       = fromList [(first, True), (second, False)]
    let found   = lookup toFound m
    assertEqual "" (Just False) found
  , testCase "find a function in a map when there is no arguments" $ do
    let first = MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = NoReturn }
    let second = MkFunctionSignature { fsName = "func", fsArgs = [], fsReturn = NoReturn }
    let toFound = MkFunctionSignature { fsName = "func", fsArgs = [], fsReturn = NoReturn }
    let m       = fromList [(first, True), (second, False)]
    let found   = lookup toFound m
    assertEqual "" (Just False) found
  , testCase "find a name args in a map when there is no arguments" $ do
    let first = MkNameArgs { naName = "sub_func", naArgs = [MkValue "arg"] }
    let second = MkNameArgs { naName = "func", naArgs = [MkValue "arg1", MkValue "arg2"] }
    let third   = MkNameArgs { naName = "main", naArgs = [] }
    let toFound = MkNameArgs { naName = "main", naArgs = [] }
    let m = fromList [(first, "first"), (second, "second"), (third, "third")]
    let found   = lookup toFound m
    assertEqual "" (Just "third") found
  ]
