module Tests
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)

import FunctionDomainDirectivesGeneratorTest (functionDomainDirectivesGeneratorTest)
import FunctionDomainAnalyticsGeneratorTest (functionDomainAnalyticsGeneratorTest)
import MainHashesTest (mainHashesTest)
import SemanticParserTest (semanticParserTest)
import SyntaxParserTest (syntaxParserTest)
import FunctionDomainAnalyticsParserWriterTest (functionDomainAnalyticsParserWriterTest)
import ValueSizeAnalyticsParserWriterTest (valueSizeAnalyticsParserWriterTest)
import FunctionDomainDirectivesParserWriterTest (functionDomainDirectivesParserWriterTest)
import ValueSizeAnalyticsGeneratorTest (valueSizeAnalyticsGeneratorTest)
import HashesRemappingTest (hashesRemappingTest)
import MemoryManagementDirectiveWriterParserTest (resourcesManagementDirectivesWriterParserTest)
import ExecFlowDirectivesParserWriterTest (execFlowDirectivesParserWriterTest)
import ExecFlowAnalyticsGeneratorTest (execFlowAnalyticsGeneratorTest)
import ExecFlowAnalyticsParserWriterTest (execFlowAnalyticsParserWriterTest)
import SemanticTreeTest (semanticTreeTest)

tests :: TestTree
tests = testGroup
  "pure tests"
  [ semanticTreeTest
  , syntaxParserTest
  , semanticParserTest
  , execFlowAnalyticsGeneratorTest
  , execFlowAnalyticsParserWriterTest
  , execFlowDirectivesParserWriterTest
  , functionDomainDirectivesGeneratorTest
  , functionDomainDirectivesParserWriterTest
  , functionDomainAnalyticsGeneratorTest
  , functionDomainAnalyticsParserWriterTest
  , valueSizeAnalyticsParserWriterTest
  , valueSizeAnalyticsGeneratorTest
  , resourcesManagementDirectivesWriterParserTest
  , mainHashesTest
  , hashesRemappingTest
  ]
