module MemoryManagementDirectiveWriterParserTest
  ( resourcesManagementDirectivesWriterParserTest
  ) where

import Prelude ()
import AspectsTestArbitrary ()
import Test.Tasty (TestTree, testGroup)
import Data.Map as M (Map, fromList, fromList, toList, empty)
import Test.Tasty.HUnit (testCase, assertEqual)
import Data.Function (($), (.), id)
import Data.Maybe (Maybe(Just, Nothing), Maybe(Nothing), isJust)
import MemoryManagementDirectiveWriters (writeMemoryManagementDirectivesMap)
import Data.Either (Either(Right))
import Errors (Errors)
import Location (Ranged(rItem))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(arbitrary), elements, vectorOf, oneof, choose, suchThat)
import Data.Eq (Eq((==)))
import MemoryManagementDirectivesParser (resourcesManagementDirectives)
import Control.Monad.Except (runExceptT, mapExceptT)
import Control.Monad (Monad(return))
import Control.Monad.Identity (Identity(runIdentity))
import Panic (panic)
import Text.Show (Show(show))
import Data.Text (Text, pack)
import ParserWrapper (parse)
import GHC.Num (Num((-)))
import Data.Bool (not, (||), (&&), Bool(False))
import SemanticTree
  ( SemanticTree(MkSemanticTree)
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Return(NoReturn, Return)
  , Arg(MkValue)
  , unpackArg
  )
import AspectsTestShared (genSemanticTree, maxItemsSize)
import Debug.Trace (trace)
import Data.Ord (Ord(compare))
import Data.Bifunctor (Bifunctor(second))
import Data.List as Lst
  (foldl, map, null, filter, (++), map, length, null, filter, elem, maximum, sortBy, sort, groupBy, head, foldl, any)
import Data.Set as S
import SemanticTreeArbitrary ()
import MemoryManagementDirectivesData
  ( MemoryManagementDirective(AllocHere, AllocAbove)
  , StatementMemoryManagementDirectives(MkStatementMemoryManagementDirectives, statementPos, statementMemoryManagement)
  , ExpressionMemoryManagementDirectives
    ( MkCallMemoryManagementDirectives
    , functionSignature
    , argumentsManagement
    , returnManagement
    )
  )

findMaxPos a =
  let calls = a in let positions = Lst.map statementPos calls in if Lst.null positions then 0 else maximum positions

resourcesManagementDirectivesWriterParserTest :: TestTree
resourcesManagementDirectivesWriterParserTest = testGroup
  "Memory management directives writer and parser tests"
  [ testCase "write empty file for empty directives"
    $ let
        source = M.empty
        result = writeMemoryManagementDirectivesMap (MkSemanticTree M.empty) source
      in assertEqual "" "" result
  , testCase "write analytics for single function without args and no return"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "main", fsArgs = [], fsReturn = NoReturn }
            , [ MkStatementMemoryManagementDirectives
                  { statementPos              = 1
                  , statementMemoryManagement =
                    [ MkCallMemoryManagementDirectives
                        { functionSignature = MkFunctionSignature { fsName = "func", fsArgs = [], fsReturn = NoReturn }
                        , argumentsManagement = M.empty
                        , returnManagement = Nothing
                        }
                    ]
                  }
              ]
            )
          ]
        result = writeMemoryManagementDirectivesMap (genSemanticTree findMaxPos source) source
      in assertEqual "" "main ~ \n|\t1 func" result
  , testCase "write analytics for single function with args and no return"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "main", fsArgs = [], fsReturn = NoReturn }
            , [ MkStatementMemoryManagementDirectives
                  { statementPos              = 1
                  , statementMemoryManagement =
                    [ MkCallMemoryManagementDirectives
                        { functionSignature   = MkFunctionSignature
                          { fsName   = "func"
                          , fsArgs   = [MkValue "a", MkValue "b", MkValue "c"]
                          , fsReturn = NoReturn
                          }
                        , argumentsManagement = M.fromList [(0, AllocHere), (1, AllocAbove), (2, AllocHere)]
                        , returnManagement    = Nothing
                        }
                    ]
                  }
              ]
            )
          ]
        result = writeMemoryManagementDirectivesMap (genSemanticTree findMaxPos source) source
      in assertEqual "" "main ~ \n\t1 func a b c ~ a = here, b = up, c = here" result
  , testCase "write analytics for single function with args and return"
    $ let
        source = M.fromList
          [ ( MkFunctionSignature { fsName = "main", fsArgs = [], fsReturn = NoReturn }
            , [ MkStatementMemoryManagementDirectives
                  { statementPos              = 1
                  , statementMemoryManagement =
                    [ MkCallMemoryManagementDirectives
                        { functionSignature   = MkFunctionSignature
                          { fsName   = "func"
                          , fsArgs   = [MkValue "a", MkValue "b", MkValue "c"]
                          , fsReturn = Return "r"
                          }
                        , argumentsManagement = M.fromList [(0, AllocHere), (1, AllocAbove), (2, AllocHere)]
                        , returnManagement    = Just AllocHere
                        }
                    ]
                  }
              ]
            )
          ]
        result = writeMemoryManagementDirectivesMap (genSemanticTree findMaxPos source) source
      in assertEqual "" "main ~ \n\t1 func a b c -> r ~ a = here, b = up, c = here, r = here" result
  , testProperty "read the same resources management directives as were written" $ \writtenConstraints ->
    let
      writtenDirectives = M.fromList $ Lst.map (second groupByCallPos) writtenConstraints
      body = --trace ("\nwrittenDirectives=" ++ show writtenDirectives) $
        writeMemoryManagementDirectivesMap (genSemanticTree findMaxPos writtenDirectives) writtenDirectives
      readDirectives = --trace ("\nbody=" ++ show body) 
        testParseAspect body
      filteredWrittenDirectives = filterEmptyDirectives writtenDirectives
    in 
      -- trace
      --   (  "\n\nreadDirectives=            "
      --   ++ show readDirectives
      --   ++ "\nfilteredWrittenDirectives= "
      --   ++ show filteredWrittenDirectives
      --   ++ "\nbody: "++show body
      --   )
      -- $
       filteredWrittenDirectives == readDirectives
  ]

groupByCallPos :: [StatementMemoryManagementDirectives] -> [StatementMemoryManagementDirectives]
groupByCallPos ungroupedLineCallsMemoryManagementDirectives =
  let
    groupedLineCallsMemoryManagementDirectives =
      groupBy (\left right -> statementPos left == statementPos right)
        $ sortBy (\l r -> compare (statementPos l) (statementPos r)) ungroupedLineCallsMemoryManagementDirectives
    lineCallsMemoryManagementDirectives = Lst.map
      (\a -> MkStatementMemoryManagementDirectives
        { statementPos              = statementPos $ head a
        , statementMemoryManagement = Lst.foldl (\acc curr -> acc ++ statementMemoryManagement curr) [] a
        }
      )
      groupedLineCallsMemoryManagementDirectives
  in lineCallsMemoryManagementDirectives

filterEmptyDirectives a = M.fromList $ Lst.map
  (second
    (Lst.filter (not . Lst.null . statementMemoryManagement) . Lst.map
      (\r -> MkStatementMemoryManagementDirectives
        { statementPos              = statementPos r
        , statementMemoryManagement =
          Lst.filter (\crm -> not (Lst.null (argumentsManagement crm) && returnManagement crm == Nothing))
            $ statementMemoryManagement r
        }
      )
    )
  )
  (M.toList a)

instance {-# OVERLAPPING #-} Arbitrary [(FunctionSignature,[StatementMemoryManagementDirectives])] where
  arbitrary = do
    size <- elements [1 .. maxItemsSize]
    vectorOf size arbitrary
    -- suchThat (return $(Lst.filter
    --   (\(s, d) -> not (Lst.null d) && not (Lst.null $fsArgs s) && (case fsReturn s of NoReturn -> False; Return r -> "" == r))) result)  (not . Lst.null)

-- instance {-# OVERLAPPING #-} Arbitrary (FunctionSignature,[LineCallsMemoryManagementDirectives]) where
--   arbitrary = do
--     signature  <- arbitrary
--     directives <- arbitrary
--     return (signature, directives)

instance {-# OVERLAPPING #-} Arbitrary [StatementMemoryManagementDirectives] where
  arbitrary = do
    size <- elements [1 .. maxItemsSize]
    vectorOf size arbitrary

instance Arbitrary StatementMemoryManagementDirectives where
  arbitrary = do
    size                      <- elements [1 .. maxItemsSize]
    statementPos              <- choose (1, size)
    statementMemoryManagement <- vectorOf size arbitrary
    return MkStatementMemoryManagementDirectives { statementPos, statementMemoryManagement }


instance Arbitrary ExpressionMemoryManagementDirectives where
  arbitrary = do
    functionSignature <- arbitrary
    let ret  = fsReturn functionSignature
    let args = fsArgs functionSignature
    let size = length (fsArgs functionSignature)
    argumentsManagementList <- vectorOf size $ do
      pos       <- elements [0 .. size - 1]
      directive <- arbitrary
      return (pos, directive)
    let argumentsManagement = M.fromList argumentsManagementList
    returnManagementArb <- arbitrary
    let
      returnManagement = case fsReturn functionSignature of
        Return _ -> returnManagementArb
        NoReturn -> Nothing
    return MkCallMemoryManagementDirectives { functionSignature, argumentsManagement, returnManagement }

instance Arbitrary MemoryManagementDirective where
  arbitrary = oneof [return AllocHere, return AllocAbove]

extractRight _    (Right r) = r
extractRight body other     = panic ("\nunexpected: " ++ show other ++ "\nbody: " ++ show body)

testParseAspect :: Text -> M.Map FunctionSignature [StatementMemoryManagementDirectives]
testParseAspect body =
  let
    parsed = parse "" body resourcesManagementDirectives
    eitherResult =
      (runExceptT $ mapExceptT (return . runIdentity) parsed) :: Either
          Errors
          (Either Errors [Ranged (FunctionSignature, [StatementMemoryManagementDirectives])])
  in M.fromList $ Lst.map rItem $ extractRight body $ extractRight body eitherResult
