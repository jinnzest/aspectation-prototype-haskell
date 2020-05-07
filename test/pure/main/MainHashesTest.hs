module MainHashesTest
  ( mainHashesTest
  ) where

import AspectsTestArbitrary ()
import SemanticTreeArbitrary ()
import AspectsTestShared (maxItemsSize)
import Data.Text (Text, pack, unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCaseSteps)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, elements, oneof, shrink, testProperty, vectorOf)

import Data.Bifunctor (first, Bifunctor(bimap))
import Data.Map as M (Map, fromList, toList)
import Data.List (intercalate, sortBy, sort)
import Debug.Trace (trace)
import MainHashes (parseHashesSyntax, writeMainHashes)
import SemanticTree
  ( Arg(MkValue)
  , FunctionSignature(MkFunctionSignature)
  , NameArgs(MkNameArgs)
  , Return(NoReturn, Return)
  , FunctionHash(MkFunctionHash, functionHash, statementHashes)
  , fsArgs
  , fsName
  , fsReturn
  , naArgs
  , naName
  )
import ShowJ (showJ)

mainHashesTest :: TestTree
mainHashesTest = testGroup
  "main hashes test"
  [ testProperty "read the same function signatures and hashes as were written" $ \hashesSignatures ->
      let
        hashSignaturesMap = M.fromList hashesSignatures
        body              = writeMainHashes hashSignaturesMap
        parsedSignatures  = parseHashesSyntax "" body
        initialSorted     = sort (M.toList hashSignaturesMap)
        parsedSorted      = sort (M.toList parsedSignatures)
        result = -- trace ("\ninitial: "++show hashesSignatures++"\nparsed: "++show parsedSignatures) $
          --trace (-- "\ninitialSorted: "++show initialSorted++
                 -- "\nbody: " ++ show body -- ++"\nparsedSorted: "++show parsedSorted
           --                             )$
          initialSorted == parsedSorted
      in result
  ]

instance {-# OVERLAPPING #-} Arbitrary [(FunctionSignature, FunctionHash)] where
  arbitrary = do
    size <- elements [0 .. maxItemsSize]
    vectorOf size arbitrary
