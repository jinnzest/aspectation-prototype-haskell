module ValueSizeAnalyticsParserWriterTest
  ( valueSizeAnalyticsParserWriterTest
  ) where

import Data.Map as M (fromList, toList, Map, empty)
import Data.Set as S (empty, fromList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import ValueSizeWriters (writeValueSizeAnalyticsMap)
import SemanticTree
  ( FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Return(Return, NoReturn)
  , Arg(MkValue)
  , SemanticTree(MkSemanticTree)
  )
import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName))
import ValueSizeData
  ( ValueSizeAnalyticsValues(MkValueSizeAnalyticsValues, argumentSizes, resultSize, callArgSizes, internalSizes)
  , ValueSizeAnalyticsValue(Pointer, Bits)
  , ValueSizeAnalyticsValue(Bits)
  , PositionedFunctionCall(MkPositionedFunctionCall, callPos, functionCalls)
  , FunctionCallWithArgSizes(MkFunctionCallWithSizes, argSizes, retSize, functionCall)
  )
import TestShared (testParse)
import Data.Text (pack)

valueSizeAnalyticsParserWriterTest :: TestTree
valueSizeAnalyticsParserWriterTest = testGroup
  "Function domain analytics parser and writer tests"
  [ testCase "write empty file for empty analytics"
    $ let
        source = M.empty
        result = writeValueSizeAnalyticsMap (MkSemanticTree M.empty) source
      in assertEqual "" "" result
  , testCase "write analytics for single function without args and no return"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "func", fsArgs = [], fsReturn = NoReturn }
            , M.fromList
              [ ( M.empty
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.empty
                  , internalSizes = M.empty
                  , resultSize    = Nothing
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          ]
        result = writeValueSizeAnalyticsMap (MkSemanticTree M.empty) source
      in assertEqual "" "func ~ " result
  , testCase "write analytics for single function with pointer only args and no return"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = NoReturn }
            , M.fromList
              [ ( M.fromList [(0, Pointer), (1, Pointer)]
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.fromList [(0, Pointer), (1, Pointer)]
                  , internalSizes = M.empty
                  , resultSize    = Nothing
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          ]
        result = writeValueSizeAnalyticsMap (MkSemanticTree M.empty) source
      in assertEqual "" "func a b ~ a= pointer, b= pointer" result
  , testCase "write analytics for single function with bits only args and no return"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = NoReturn }
            , M.fromList
              [ ( M.fromList [(0, Bits 1), (1, Bits 2)]
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.fromList [(0, Bits 1), (1, Bits 2)]
                  , internalSizes = M.empty
                  , resultSize    = Nothing
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          ]
        result = writeValueSizeAnalyticsMap (MkSemanticTree M.empty) source
      in assertEqual "" "func a b ~ a= 1 bit, b= 2 bits" result
  , testCase "write analytics for single function with bits only args and returning pointer"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = Return "r" }
            , M.fromList
              [ ( M.fromList [(0, Bits 1), (1, Bits 2)]
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.fromList [(0, Bits 1), (1, Bits 2)]
                  , internalSizes = M.empty
                  , resultSize    = Just Pointer
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          ]
        result = writeValueSizeAnalyticsMap (MkSemanticTree M.empty) source
      in assertEqual "" "func a b -> r ~ a= 1 bit, b= 2 bits, r= pointer" result
  , testCase "write analytics for single function with bits only args and returning 3 bits"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "func", fsArgs = [MkValue "a", MkValue "b"], fsReturn = Return "r" }
            , M.fromList
              [ ( M.fromList [(0, Bits 1), (1, Bits 2)]
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.fromList [(0, Bits 1), (1, Bits 2)]
                  , internalSizes = M.empty
                  , resultSize    = Just $ Bits 3
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          ]
        result = writeValueSizeAnalyticsMap (MkSemanticTree M.empty) source
      in assertEqual "" "func a b -> r ~ a= 1 bit, b= 2 bits, r= 3 bits" result
  , testCase "write analytics for single function with function calls chain"
    $ let
        tree =
          testParse "fn main =\n\tfunc4\n\tfunc2 3 func3 1\n\tfunc4\nfn func2 a b = a\nfn func3 a = a\nfn func4 = 1"
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "main", fsArgs = [], fsReturn = Return "r" }
            , M.fromList
              [ ( M.empty
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.empty
                  , internalSizes = M.empty
                  , resultSize    = Just $ Bits 2
                  , callArgSizes  = S.fromList
                    [ MkPositionedFunctionCall
                      { callPos       = 1
                      , functionCalls =
                        [ MkFunctionCallWithSizes
                            { argSizes     = []
                            , retSize      = Just $ Bits 2
                            , functionCall = MkFunctionCall { fcName = "func4", fcArgs = [], tmpVarName = Nothing }
                            }
                        ]
                      }
                    , MkPositionedFunctionCall
                      { callPos       = 2
                      , functionCalls =
                        [ MkFunctionCallWithSizes
                          { argSizes     = [Bits 2, Bits 2]
                          , retSize      = Just $ Bits 2
                          , functionCall = MkFunctionCall
                            { fcName     = "func2"
                            , fcArgs     = ["a", "b"]
                            , tmpVarName = Nothing
                            }
                          }
                        , MkFunctionCallWithSizes
                          { argSizes     = [Bits 2]
                          , retSize      = Just $ Bits 2
                          , functionCall = MkFunctionCall { fcName = "func3", fcArgs = ["a"], tmpVarName = Nothing }
                          }
                        ]
                      }
                    , MkPositionedFunctionCall
                      { callPos       = 3
                      , functionCalls =
                        [ MkFunctionCallWithSizes
                            { argSizes     = []
                            , retSize      = Just $ Bits 2
                            , functionCall = MkFunctionCall { fcName = "func4", fcArgs = [], tmpVarName = Nothing }
                            }
                        ]
                      }
                    ]
                  }
                )
              ]
            )
          , ( MkFunctionSignature { fsName = "func2", fsArgs = [MkValue "a", MkValue "b"], fsReturn = Return "r" }
            , M.fromList
              [ ( M.fromList [(0, Bits 2), (1, Bits 2)]
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.fromList [(0, Bits 2), (1, Bits 2)]
                  , internalSizes = M.empty
                  , resultSize    = Just $ Bits 2
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          , ( MkFunctionSignature { fsName = "func3", fsArgs = [MkValue "a"], fsReturn = Return "r" }
            , M.fromList
              [ ( M.fromList [(0, Bits 2)]
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.fromList [(0, Bits 2)]
                  , internalSizes = M.empty
                  , resultSize    = Just $ Bits 2
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          , ( MkFunctionSignature { fsName = "func4", fsArgs = [], fsReturn = Return "r" }
            , M.fromList
              [ ( M.empty
                , MkValueSizeAnalyticsValues
                  { argumentSizes = M.empty
                  , internalSizes = M.empty
                  , resultSize    = Just $ Bits 2
                  , callArgSizes  = S.empty
                  }
                )
              ]
            )
          ]
        result = writeValueSizeAnalyticsMap tree source
      in assertEqual
        ""
        (pack $ concat
          [ "func2 a b -> r ~ a= 2 bits, b= 2 bits, r= 2 bits"
          , "\nfunc3 a -> r ~ a= 2 bits, r= 2 bits"
          , "\nfunc4 -> r ~ r= 2 bits"
          , "\nmain -> r ~ r= 2 bits"
          , "\n\t1 func4  ~  -> 2 bits"
          , "\n|\t2 func2 3 func3 1"
          , "\n\t2 func3 a ~ a= 2 bits -> 2 bits"
          , "\n\t2 func2 a b ~ a= 2 bits, b= 2 bits -> 2 bits"
          , "\n\t3 func4  ~  -> 2 bits"
          ]
        )
        result
  ]
