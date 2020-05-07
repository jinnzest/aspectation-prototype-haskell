module HashesRemappingTest
  ( hashesRemappingTest
  ) where

import Prelude ()
import Test.Tasty (TestTree, testGroup)
import SemanticTree
  ( FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , FunctionHash(MkFunctionHash, functionHash, statementHashes)
  , SemanticModel(MkSemanticModel, tree)
  , SemanticTree(MkSemanticTree, _semanticTree)
  , Return(NoReturn)
  , Function(MkFunction, fBody, fSignature)
  , Arg(MkValue)
  )
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, assertEqual)
import HashesRemapping (mapOldToNewNames)
import Data.Map (empty, fromList)
import Data.Function (($))

hashesRemappingTest :: TestTree
hashesRemappingTest = testGroup
  "hashes remapping"
  [ testCase "empty old and new names produce empty names" $ do
    let result = mapOldToNewNames empty MkSemanticModel { tree = MkSemanticTree { _semanticTree = empty } }
    assertEqual "results mismatch" empty result
  , testCase "empty old and new names produce empty names2" $ do
    let
      result = mapOldToNewNames
        (fromList
          [ ( MkFunctionSignature { fsName = "some name", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          ]
        )
        MkSemanticModel { tree = MkSemanticTree { _semanticTree = empty } }
    assertEqual "results mismatch" empty result
  , testCase "empty old and new names produce empty names3" $ do
    let
      result = mapOldToNewNames
        empty
        MkSemanticModel
          { tree = MkSemanticTree
            { _semanticTree = fromList
              [ ( MkFunctionHash { functionHash = "some hash", statementHashes = [] }
                , [ MkFunction
                      { fSignature = MkFunctionSignature { fsName = "some name", fsArgs = [], fsReturn = NoReturn }
                      , fBody      = []
                      }
                  ]
                )
              ]
            }
          }
    assertEqual "results mismatch" empty result
  , testCase "map old to new name" $ do
    let
      result = mapOldToNewNames
        (fromList
          [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          ]
        )
        MkSemanticModel
          { tree = MkSemanticTree
            { _semanticTree = fromList
              [ ( MkFunctionHash { functionHash = "some hash", statementHashes = [] }
                , [ MkFunction
                      { fSignature = MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }
                      , fBody      = []
                      }
                  ]
                )
              ]
            }
          }
    assertEqual
      "results mismatch"
      (fromList
        [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
          , [MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }]
          )
        ]
      )
      result
  , testCase "map old to new name skipping a removed name" $ do
    let
      result = mapOldToNewNames
        (fromList
          [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          , ( MkFunctionSignature { fsName = "old name2", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash2", statementHashes = [] }
            )
          ]
        )
        MkSemanticModel
          { tree = MkSemanticTree
            { _semanticTree = fromList
              [ ( MkFunctionHash { functionHash = "some hash", statementHashes = [] }
                , [ MkFunction
                      { fSignature = MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }
                      , fBody      = []
                      }
                  ]
                )
              ]
            }
          }
    assertEqual
      "results mismatch"
      (fromList
        [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
          , [MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }]
          )
        ]
      )
      result
  , testCase "map old to new name skipping an added name" $ do
    let
      result = mapOldToNewNames
        (fromList
          [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          ]
        )
        MkSemanticModel
          { tree = MkSemanticTree
            { _semanticTree = fromList
              [ ( MkFunctionHash { functionHash = "some hash", statementHashes = [] }
                , [ MkFunction
                      { fSignature = MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }
                      , fBody      = []
                      }
                  ]
                )
              , ( MkFunctionHash { functionHash = "some hash2", statementHashes = [] }
                , [ MkFunction
                      { fSignature = MkFunctionSignature { fsName = "new name2", fsArgs = [], fsReturn = NoReturn }
                      , fBody      = []
                      }
                  ]
                )
              ]
            }
          }
    assertEqual
      "results mismatch"
      (fromList
        [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
          , [MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }]
          )
        ]
      )
      result
  , testCase "map old to 2 new names" $ do
    let
      result = mapOldToNewNames
        (fromList
          [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          ]
        )
        MkSemanticModel
          { tree = MkSemanticTree
            { _semanticTree = fromList
              [ ( MkFunctionHash { functionHash = "some hash", statementHashes = [] }
                , [ MkFunction
                    { fSignature = MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }
                    , fBody      = []
                    }
                  , MkFunction
                    { fSignature = MkFunctionSignature { fsName = "new name2", fsArgs = [], fsReturn = NoReturn }
                    , fBody      = []
                    }
                  ]
                )
              ]
            }
          }
    assertEqual
      "results mismatch"
      (fromList
        [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
          , [ MkFunctionSignature { fsName = "new name2", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }
            ]
          )
        ]
      )
      result
  , testCase "map 2 old names to a new one" $ do
    let
      result = mapOldToNewNames
        (fromList
          [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          , ( MkFunctionSignature { fsName = "old name2", fsArgs = [], fsReturn = NoReturn }
            , MkFunctionHash { functionHash = "some hash", statementHashes = [] }
            )
          ]
        )
        MkSemanticModel
          { tree = MkSemanticTree
            { _semanticTree = fromList
              [ ( MkFunctionHash { functionHash = "some hash", statementHashes = [] }
                , [ MkFunction
                      { fSignature = MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }
                      , fBody      = []
                      }
                  ]
                )
              ]
            }
          }
    assertEqual
      "results mismatch"
      (fromList
        [ ( MkFunctionSignature { fsName = "old name", fsArgs = [], fsReturn = NoReturn }
          , [MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }]
          )
        , ( MkFunctionSignature { fsName = "old name2", fsArgs = [], fsReturn = NoReturn }
          , [MkFunctionSignature { fsName = "new name", fsArgs = [], fsReturn = NoReturn }]
          )
        ]
      )
      result
  ]
