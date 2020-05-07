module IOTests
  ( ioTests
  ) where

import Test.Tasty (TestTree, testGroup)

import IRGenerationTest (irGenerationTest)

ioTests :: TestTree
ioTests = testGroup "IO tests" [irGenerationTest]
