module SemanticParserTest
  ( semanticParserTest
  ) where

import Control.Monad.Except (runExcept)
import Data.Map as M (empty, fromList, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Maybe (Maybe(Nothing))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Megaparsec.Pos (SourcePos(SourcePos), mkPos, sourceColumn, sourceLine, sourceName)

import EmbeddedFuncs (EmbeddedFuncs(MkEmbeddedFuncs), printF)
import Errors (Error(MkError, MkMultiRangedError, MkRangedError), Errors(MkErrors), errors)
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
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, tmpVarName)
  , Function(MkFunction)
  , FunctionSignature(MkFunctionSignature)
  , NameArgs(MkNameArgs)
  , Return(NoReturn, Return)
  , FunctionHash(MkFunctionHash, functionHash, statementHashes)
  , ExpressionHash(MkExpressionHash, expressionHash, semanticPlace)
  , SemanticPlace(MkSemanticPlace, statementPlace, expressionPlace)
  , SemanticModel(MkSemanticModel)
  , SemanticTree(MkSemanticTree)
  , SemanticTree(MkSemanticTree)
  , Statement(MkNoAssignment, expression)
  , _semanticTree
  , fBody
  , fSignature
  , fcsArgs
  , fcsName
  , fsArgs
  , fsName
  , fsReturn
  , naArgs
  , naName
  , tree
  )
import SyntaxParser (parseSyntax)
import TestShared (parseStep, testParse, testParseError, mkRange)
import Debug.Trace (trace)
import SemanticTreeArbitrary (mkRanged)
import Data.Map.Strict (toList)

semanticParserTest :: TestTree
semanticParserTest = testGroup
  "semantic parser test"
  [ testCase "no main function error when single non main function exists" $ do
    let src    = "fn some_func = 1"
    let result = testParseError src
    assertEqual "" [MkError "main function is missing"] result
  , testCase "main function exists only" $ do
    let src    = "fn main = 1"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "e02cce2b7abf1d870de40c48c01b64a4d46a8ba6b118e25e26eba3eddd4608ac"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "551e8b15be547418fbf59dbfdb9c1788e0846b7994be099e6e0e9960ff26457f"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "main"
                    , Sem.fsArgs   = []
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ ranged (1, 11) (1, 12) $ Sem.MkNoAssignment $ Sem.MkConstantInteger
                        (Just "a")
                        MkRanged { rItem = 1, range = mkRange (1, 11) (1, 12) }
                    ]
                  }
              ]
            )
          ]
        }
      result
  , testCase "main function and another one exist" $ do
    let src          = "\nfn another = 1\nfn main = another"
    let result       = testParse src
    let rangeAnother = mkRange (3, 11) (3, 18)
    let rangeConst   = mkRange (2, 14) (2, 15)
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "dfc0d346a15a117fbc2aea599bb4b892520ca7d40a7cef4d5eefd560bee4f371"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "50edfc02f16bb70e9df1a25e5d327f730b7a6578a13758e28c251e790c4208f4"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "main"
                    , Sem.fsArgs   = []
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ MkRanged
                        { rItem = Sem.MkNoAssignment $ Sem.MkFunctionCall
                          { Sem.fcsName    = MkRanged { rItem = "another", range = rangeAnother }
                          , Sem.fcsArgs    = []
                          , Sem.tmpVarName = Just "a"
                          }
                        , range = rangeAnother
                        }
                    ]
                  }
              ]
            )
          , ( Sem.MkFunctionHash
              { functionHash    = "e02cce2b7abf1d870de40c48c01b64a4d46a8ba6b118e25e26eba3eddd4608ac"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "551e8b15be547418fbf59dbfdb9c1788e0846b7994be099e6e0e9960ff26457f"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "another"
                    , Sem.fsArgs   = []
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ MkRanged
                        { rItem = Sem.MkNoAssignment $ Sem.MkConstantInteger (Just "a") $ MkRanged
                          { rItem = 1
                          , range = rangeConst
                          }
                        , range = rangeConst
                        }
                    ]
                  }
              ]
            )
          ]
        }
      result
  , testCase "two functions with different names but equal body of constant integers has equal hashes" $ do
    let src    = "\nfn main = 1\nfn another = 1"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "e02cce2b7abf1d870de40c48c01b64a4d46a8ba6b118e25e26eba3eddd4608ac"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "551e8b15be547418fbf59dbfdb9c1788e0846b7994be099e6e0e9960ff26457f"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "another"
                  , Sem.fsArgs   = []
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (3, 14) (3, 15) $ Sem.MkNoAssignment $ Sem.MkConstantInteger (Just "a") $ MkRanged
                      { rItem = 1
                      , range = mkRange (3, 14) (3, 15)
                      }
                  ]
                }
              , Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "main"
                  , Sem.fsArgs   = []
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (2, 11) (2, 12) $ Sem.MkNoAssignment $ Sem.MkConstantInteger (Just "a") $ MkRanged
                      { rItem = 1
                      , range = mkRange (2, 11) (2, 12)
                      }
                  ]
                }
              ]
            )
          ]
        }
      result
  , testCase "two functions with different names but equal body of the function argument has equal hashes" $ do
    let src = "\nfn main arg1 arg2 = arg1\nfn another arg2 arg1 = arg2"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "df1a82483513211b9d4a9feefccb7f9b0c85c27a5add59bdc309dcedeabd7db6"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "91ce60c07767c8c5d1609a81030747f3d117d49ed19b4ffc52790085045e5d6d"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "another"
                  , Sem.fsArgs   = [MkValue "arg2", MkValue "arg1"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (3, 24) (3, 28) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "a") $ MkRanged
                      { rItem = 0
                      , range = mkRange (3, 24) (3, 28)
                      }
                  ]
                }
              , Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "main"
                  , Sem.fsArgs   = [MkValue "arg1", MkValue "arg2"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (2, 21) (2, 25) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "a") $ MkRanged
                      { rItem = 0
                      , range = mkRange (2, 21) (2, 25)
                      }
                  ]
                }
              ]
            )
          ]
        }
      result
  , testCase "generated tmp variable names don't clash with the function argument names" $ do
    let src    = "\nfn main a b c = a\nfn another a b = a"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "df1a82483513211b9d4a9feefccb7f9b0c85c27a5add59bdc309dcedeabd7db6"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "91ce60c07767c8c5d1609a81030747f3d117d49ed19b4ffc52790085045e5d6d"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "another"
                  , Sem.fsArgs   = [MkValue "a", MkValue "b"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (3, 18) (3, 19) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "c") $ MkRanged
                      { rItem = 0
                      , range = mkRange (3, 18) (3, 19)
                      }
                  ]
                }
              , Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "main"
                  , Sem.fsArgs   = [MkValue "a", MkValue "b", MkValue "c"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (2, 17) (2, 18) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "d") $ MkRanged
                      { rItem = 0
                      , range = mkRange (2, 17) (2, 18)
                      }
                  ]
                }
              ]
            )
          ]
        }
      result
  , testCase "generated tmp variable names of two functions can clash" $ do
    let src    = "\nfn main a b c = a\nfn another a b c= a"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "df1a82483513211b9d4a9feefccb7f9b0c85c27a5add59bdc309dcedeabd7db6"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "91ce60c07767c8c5d1609a81030747f3d117d49ed19b4ffc52790085045e5d6d"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "another"
                  , Sem.fsArgs   = [MkValue "a", MkValue "b", MkValue "c"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (3, 19) (3, 20) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "d") $ MkRanged
                      { rItem = 0
                      , range = mkRange (3, 19) (3, 20)
                      }
                  ]
                }
              , Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "main"
                  , Sem.fsArgs   = [MkValue "a", MkValue "b", MkValue "c"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (2, 17) (2, 18) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "d") $ MkRanged
                      { rItem = 0
                      , range = mkRange (2, 17) (2, 18)
                      }
                  ]
                }
              ]
            )
          ]
        }
      result
  , testCase "two functions with different names but equal body calling the same function have equal hashes" $ do
    let src = "\nfn main arg1 arg2 = to_call arg2\nfn another arg1 arg2 = to_call arg2\nfn to_call arg1 = arg1"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "e1785f5106b2304fec694374773e5ba78d090afa4c1279025ea183b3380130b5"
              , statementHashes =
                [ [ MkExpressionHash
                    { expressionHash = "3996c975af99d75fc9e55ff24c928e11f866d2fda4f0be627a1da9f9230239a6"
                    , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                    }
                  , MkExpressionHash
                    { expressionHash = "85d1387ed3dc664b279d210b13f54b0367a470fc2e780c5e646c74bc88e04ca4"
                    , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                    }
                  ]
                ]
              }
            , [ Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "another"
                  , Sem.fsArgs   = [MkValue "arg1", MkValue "arg2"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (3, 24) (3, 36) $ Sem.MkNoAssignment $ Sem.MkFunctionCall
                      { Sem.fcsName    = ranged (3, 24) (3, 31) "to_call"
                      , Sem.fcsArgs    =
                        [Sem.MkFunctionArgument (Just "a") $ MkRanged { rItem = 1, range = mkRange (3, 32) (3, 36) }]
                      , Sem.tmpVarName = Just "a"
                      }
                  ]
                }
              , Sem.MkFunction
                { Sem.fSignature = Sem.MkFunctionSignature
                  { Sem.fsName   = "main"
                  , Sem.fsArgs   = [MkValue "arg1", MkValue "arg2"]
                  , Sem.fsReturn = Return "r"
                  }
                , Sem.fBody      =
                  [ ranged (2, 21) (2, 33) $ Sem.MkNoAssignment $ Sem.MkFunctionCall
                      { Sem.fcsName    = ranged (2, 21) (2, 28) "to_call"
                      , Sem.fcsArgs    =
                        [Sem.MkFunctionArgument (Just "a") $ MkRanged { rItem = 1, range = mkRange (2, 29) (2, 33) }]
                      , Sem.tmpVarName = Just "a"
                      }
                  ]
                }
              ]
            )
          , ( Sem.MkFunctionHash
              { functionHash    = "df1a82483513211b9d4a9feefccb7f9b0c85c27a5add59bdc309dcedeabd7db6"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "91ce60c07767c8c5d1609a81030747f3d117d49ed19b4ffc52790085045e5d6d"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "to_call"
                    , Sem.fsArgs   = [MkValue "arg1"]
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ ranged (4, 19) (4, 23) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "a") $ MkRanged
                        { rItem = 0
                        , range = mkRange (4, 19) (4, 23)
                        }
                    ]
                  }
              ]
            )
          ]
        }
      result
  , testCase "call a function having call to another function in place of the first argument" $ do
    let src = "\nfn main = func sub_func 1 2\nfn func arg1 arg2 = \n\targ1\n\targ2\nfn sub_func arg1 = arg1"
    let result = testParse src
    assertEqual
      ""
      Sem.MkSemanticTree
        { Sem._semanticTree = M.fromList
          [ ( Sem.MkFunctionHash
              { functionHash    = "bb2fa948321fe33e8fb63125cc5476157f5ce447902f67e01fd23dffba9fe15b"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "91ce60c07767c8c5d1609a81030747f3d117d49ed19b4ffc52790085045e5d6d"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                , [ MkExpressionHash
                      { expressionHash = "420cf95170c6ba2f9564e904cd9da69f9c0f086a905d17d20a5fc01305b20ca4"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 2, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "func"
                    , Sem.fsArgs   = [MkValue "arg1", MkValue "arg2"]
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ ranged (4, 9) (4, 13) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument Nothing $ MkRanged
                      { rItem = 0
                      , range = mkRange (4, 9) (4, 13)
                      }
                    , ranged (5, 9) (5, 13) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "a") $ MkRanged
                      { rItem = 1
                      , range = mkRange (5, 9) (5, 13)
                      }
                    ]
                  }
              ]
            )
          , ( Sem.MkFunctionHash
              { functionHash    = "27bf98e548d70024df0eff2c8e160fc0606fb17e2adc275b8bc53927e1c273a4"
              , statementHashes =
                [ [ MkExpressionHash
                    { expressionHash = "a9c50acf54108aa23970e8f549b373985c441d785c930cfd97baa826f5a13c1d"
                    , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                    }
                  , MkExpressionHash
                    { expressionHash = "02d2f0385073469aab07ebcbd2180b43dd2a86e0ab8f9f6a40ecff7d40d259de"
                    , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                    }
                  , MkExpressionHash
                    { expressionHash = "6cf6e676e3a177aae97aea454fd49af7b2910e84746d9bedad7c01daf8c5c9f1"
                    , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                    }
                  , MkExpressionHash
                    { expressionHash = "377c9ae8e889b8d1257fa44536b74b623813443975d3617044c9eb803c6081c9"
                    , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                    }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "main"
                    , Sem.fsArgs   = []
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ ranged (2, 11) (2, 28) $ Sem.MkNoAssignment $ Sem.MkFunctionCall
                        { Sem.fcsName    = ranged (2, 11) (2, 15) "func"
                        , Sem.fcsArgs    =
                          [ Sem.MkFunctionCall
                            { Sem.fcsName    = ranged (2, 16) (2, 24) "sub_func"
                            , Sem.fcsArgs    =
                              [ Sem.MkConstantInteger (Just "a")
                                  $ MkRanged { rItem = 1, range = mkRange (2, 25) (2, 27) }
                              ]
                            , Sem.tmpVarName = Just "a"
                            }
                          , Sem.MkConstantInteger (Just "a") $ MkRanged { rItem = 2, range = mkRange (2, 27) (2, 28) }
                          ]
                        , Sem.tmpVarName = Just "a"
                        }
                    ]
                  }
              ]
            )
          , ( Sem.MkFunctionHash
              { functionHash    = "df1a82483513211b9d4a9feefccb7f9b0c85c27a5add59bdc309dcedeabd7db6"
              , statementHashes =
                [ [ MkExpressionHash
                      { expressionHash = "91ce60c07767c8c5d1609a81030747f3d117d49ed19b4ffc52790085045e5d6d"
                      , semanticPlace  = MkSemanticPlace { statementPlace = 1, expressionPlace = 1 }
                      }
                  ]
                ]
              }
            , [ Sem.MkFunction
                  { Sem.fSignature = Sem.MkFunctionSignature
                    { Sem.fsName   = "sub_func"
                    , Sem.fsArgs   = [MkValue "arg1"]
                    , Sem.fsReturn = Return "r"
                    }
                  , Sem.fBody      =
                    [ ranged (6, 20) (6, 24) $ Sem.MkNoAssignment $ Sem.MkFunctionArgument (Just "a") $ MkRanged
                        { rItem = 0
                        , range = mkRange (6, 20) (6, 24)
                        }
                    ]
                  }
              ]
            )
          ]
        }
      result
  , testCase "error when there are duplicated functions" $ do
    let src = "\nfn main arg1 arg2 = arg1\nfn another arg1 arg2 = arg1\nfn another arg1 arg2 = arg1"
    let result = testParseError src
    assertEqual
      ""
      [ MkMultiRangedError MkMultiRanged
          { mrItem = "Duplicated function 'another'"
          , ranges = [mkRange (4, 1) (4, 28), mkRange (3, 1) (3, 28)]
          }
      ]
      result
  , testCase "error when there is undefined identifier" $ do
    let src    = "fn main = undefined_identifier"
    let result = testParseError src
    assertEqual "" [MkRangedError $ ranged (1, 11) (1, 31) "The 'undefined_identifier' is undefined identifier"] result
  , testCase "error when a function call missing an arg" $ do
    let src    = "fn main = print   "
    let result = testParseError src
    assertEqual
      ""
      [MkRangedError $ ranged (1, 11) (1, 16) "Expected one more argument to the function call 'print'"]
      result
  , testCase "error " $ do
    let src    = "fn main =  1 2"
    let result = testParseError src
    assertEqual "" [MkRangedError $ ranged (1, 14) (1, 15) "Unexpected expression: '2'"] result
  , testCase "error when there is an extra arg to a function call" $ do
    let src    = "fn main = print 1 2"
    let result = testParseError src
    assertEqual "" [MkRangedError $ ranged (1, 11) (1, 16) "2 args but function 'print' expected 1 args"] result
  , testCase "error when a value is expected but sub function returns nothing" $ do
    let src    = "fn main = print subfunc\nfn subfunc = print 1"
    let result = testParseError src
    assertEqual
      ""
      [MkRangedError $ ranged (1, 17) (1, 24) "value expected but call of 'subfunc' returns nothing"]
      result
  , testCase "error when a value is expected but sub sub function returns nothing" $ do
    let src = "fn main = \n\tsubfunc\n\tprint 2\nfn subfunc = \n\tprint 3\n\tprint subsubfunc\nfn subsubfunc = print 2"
    let result = testParseError src
    assertEqual
      ""
      [MkRangedError $ ranged (6, 15) (6, 25) "value expected but call of 'subsubfunc' returns nothing"]
      result
  , testCase "error when a value in place of first arg is expected but sub sub function returns nothing" $ do
    let
      src
        = "fn main = \n\tsubfunc subsubfunc 1 2\n\tprint 2\nfn subfunc a1 a2 = \n\tprint a1\n\tprint a2\nfn subsubfunc a = print a"

    let result = testParseError src
    assertEqual
      ""
      [MkRangedError $ ranged (2, 17) (2, 27) "value expected but call of 'subsubfunc' returns nothing"]
      result
  ]

ranged (lf, cf) (lt, ct) rItem = MkRanged { rItem, range = mkRange (lf, cf) (lt, ct) }
