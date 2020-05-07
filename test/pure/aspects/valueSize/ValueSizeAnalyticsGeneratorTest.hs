module ValueSizeAnalyticsGeneratorTest
  ( valueSizeAnalyticsGeneratorTest
  ) where

import Control.Monad.Except (runExceptT, mapExceptT)
import Control.Monad.Identity (Identity(runIdentity))
import Panic (panic)
import Data.Map (fromList, toList, Map, empty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Text as T (Text, concat)

import ValueSizeWriters (writeValueSizeAnalyticsMap)
import SemanticTree
  ( FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , SemanticTree(MkSemanticTree, _semanticTree)
  , Function(fSignature)
  , Arg(MkValue)
  , Return(NoReturn)
  )
import ValueSizeData
  ( ValueSizeAnalyticsValues(MkValueSizeAnalyticsValues, argumentSizes, resultSize)
  , ValueSizeAnalyticsValue(Pointer, Bits)
  , ValueSizeAnalytics
  )
import ValueSizeAnalytics (generateValueSizeAnalytics)
import FunctionDomainTestShared (testParseFunctionDomainDirectives)
import TestShared (testParse)
import Errors (Errors)
import Location (Ranged)
import FunctionDomainAnalyticsData (UndefinedFunctionProbability)
import FunctionDomainDirectivesData (FunctionDomainDirectives)
import Debug.Trace (trace)


valueSizeAnalyticsGeneratorTest :: TestTree
valueSizeAnalyticsGeneratorTest = testGroup
  "Function domain analytics generator tests"
  [ testCase "write empty file for empty analytics"
    $ let
        source     = MkSemanticTree { _semanticTree = empty }
        directives = testParseFunctionDomainDirectives ""
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap (MkSemanticTree empty) result
      in assertEqual "" "" resultStr
  , testCase "write value size analytics for function with domain -128<=a<=5 as 1 byte"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -128<=a<=5"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 8 bits" resultStr
  , testCase "write value size analytics for function with domain -1<=a<=127 as 1 byte"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -1<=a<=127"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 8 bits" resultStr
  , testCase "write value size analytics for function with domain -1<=a<=128 as 2 bytes"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -1<=a<=128"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 16 bits" resultStr
  , testCase "write value size analytics for function with domain -129<=a<=5 as 2 bytes"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -129<=a<=5"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 16 bits" resultStr
  , testCase "write value size analytics for function with domain -1<=a<=32768 as 4 bytes"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -1<=a<=32768"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 32 bits" resultStr
  , testCase "write value size analytics for function with domain -32769<=a<=5 as 4 bytes"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -32769<=a<=5"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 32 bits" resultStr
  , testCase "write value size analytics for function with domain -2147483649<=a<=5 as 8 bytes"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -2147483649<=a<=5"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 64 bits" resultStr
  , testCase "write value size analytics for function with domain -1<=a<=2147483648 as 8 bytes"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -1<=a<=2147483648"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 64 bits" resultStr
  , testCase "write value size analytics for function with domain -1<=a<=inf as pointer"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -1<=a<=inf"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= pointer" resultStr
  , testCase "write value size analytics for function with domain -inf<=a<=1 as pointer"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ -inf<=a<=1"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= pointer" resultStr
  , testCase "write value size analytics for function with domain 0<=a<=1 as 1 bit"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ 0<=a<=1"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 1 bit" resultStr
  , testCase "write value size analytics for function with domain 0<=a<=1 as 1 bit"
    $ let
        source     = testParse "fn main a = a"
        directives = testParseFunctionDomainDirectives "main a -> r ~ 0<=a<=1"
        result     = generateValueSizeAnalytics [] source directives empty
        resultStr  = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= 1 bit" resultStr
  , testCase
      "write value size analytics for function bodies consisting of a pointer, 1 and 8 bits args and 8 bits result"
    $ let
        source =
          testParse "fn main a = \n\tf2 3\n\tprint a\n\tf1 f2 5 2\n\t1\nfn f1 a b = \n\tprint b\n\tprint a\nfn f2 x = x"
        directives =
          testParseFunctionDomainDirectives
            "main a -> r ~ 0<=a<=1, -20<=r<=20\nf1 a b ~ -inf<=a<=inf, -inf<=b<=inf\nf2 x -> r ~ -5<=x<=10, -5<=r<=10\nprint a ~ -inf<=a<=inf"
        result = generateValueSizeAnalytics
          [MkFunctionSignature { fsName = "print", fsArgs = [MkValue "a"], fsReturn = NoReturn }]
          source
          directives
          empty
        resultStr = writeValueSizeAnalyticsMap source result
      in assertEqual
        ""
        (T.concat
          [ "f1 a b ~ a= pointer, b= pointer\n\t1 print a ~ a= pointer\n\t2 print a ~ a= pointer\nf2 x -> r ~ x= 8 bits, r= 8 bits\nmain a -> r ~ a= 1 bit, r= 1 bit"
          , "\n\t1 f2 x ~ x= 8 bits\n\t2 print a ~ a= 1 bit\n|\t3 f1 f2 5 2\n\t3 f2 x ~ x= 8 bits -> 8 bits\n\t3 f1 a b ~ a= 8 bits, b= 8 bits"
          ]
        )
        resultStr
  , testCase
      "write value size analytics for function body consisting of lines with prints with args of 1 and 8 bits and returning 1 bit"
    $ let
        source = testParse "fn main a =\n\tprint 1\n\tprint 2\n\t1"
        directives =
          testParseFunctionDomainDirectives "main a -> r ~ -inf<=a<=inf, -inf<=r<=inf\nprint a ~ -inf<=a<=inf"
        result = generateValueSizeAnalytics
          [MkFunctionSignature { fsName = "print", fsArgs = [MkValue "a"], fsReturn = NoReturn }]
          source
          directives
          empty
        resultStr = writeValueSizeAnalyticsMap source result
      in assertEqual "" "main a -> r ~ a= pointer, r= 1 bit\n\t1 print a ~ a= 1 bit\n\t2 print a ~ a= 8 bits" resultStr
  , testCase "write value size analytics for function body consisting of line with a pointer arg and 8 bits result"
    $ let
        source     = testParse "fn main a = retval\nfn retval = 3\nfn retval2 = 1"
        directives = testParseFunctionDomainDirectives
          "main a -> r ~ -inf<=a<=inf, -inf<=r<=inf\nretval  -> r ~ -inf<=r<=inf\nretval2 -> r ~ -inf<=r<=inf"
        result = generateValueSizeAnalytics
          [MkFunctionSignature { fsName = "print", fsArgs = [MkValue "a"], fsReturn = NoReturn }]
          source
          directives
          empty
        resultStr = writeValueSizeAnalyticsMap source result
      in assertEqual
        ""
        "main a -> r ~ a= pointer, r= 8 bits\n\t1 retval  ~  -> 8 bits\nretval -> r ~ r= 8 bits\nretval2 -> r ~ r= 1 bit"
        resultStr
  ]
