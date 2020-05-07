import Test.Tasty (TestTree, defaultMain, testGroup)

import IOTests (ioTests)
import Tests (tests)

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests" [tests, ioTests]
