module IRGenerationTest
  ( irGenerationTest
  ) where

import Control.Monad.Except (runExceptT)
import Data.Text.IO (readFile)
import Prelude (($), (++))
import ML (runMLE)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCaseSteps)

irGenerationTest :: TestTree
irGenerationTest = testGroup
  "IR generation test"
  [ testCaseSteps "generating LLVM IR files" $ \step -> do
      step "run the multi-language"
      runExceptT $ runMLE testDir
      step "compare reference and generated IR files"
      reference <- readReference
      generated <- readGenerated
      assertEqual "" reference generated
  ]

readReference = readFile $ testDir ++ "/ref_out.ll"

readGenerated = readFile $ testDir ++ "/target/out.ll"

testDir = "tests_data/ir"
