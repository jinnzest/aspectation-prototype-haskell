module FunctionDomainArbitrary
  () where

import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, testProperty, suchThat, oneof, choose)
import FunctionDomainAnalyticsData
  ( UndefinedProbability(MaybeDefined, Defined)
  , CallProbability(MaybeWillBeCalled, WillBeCalled)
  , FunctionCallWithProbability(MkFunctionCallWithProbability, callProbability, functionCall)
  , PositionedFunctionCall(callPos, functionCalls)
  , PositionedFunctionCall(MkPositionedFunctionCall)
  , UndefinedFunctionProbability(MkUndefinedFunctionProbability, callUndefinedProbabilities, resultUndefinedProbability)
  )
import AspectsTestShared (maxItemsSize)
import AspectsTestArbitrary ()
import TestShared (setOf)
import qualified Data.Set as S
import Data.List (groupBy)
import SemanticTree
  (FunctionSignature(fsReturn, fsArgs, MkFunctionSignature, fsName), Return(NoReturn, Return), Arg(MkValue))
import SemanticTreeArbitrary ()
import FunctionDomainDirectivesData as FDD
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainConstraintFrom(MkNegInfinite)
  , FunctionDomainConstraintTo(MkPosInfinite)
  , from
  , input
  , output
  , to
  )
import qualified Data.Map as M
import Data.Text (pack)
import Debug.Trace (trace)

instance Arbitrary UndefinedProbability where
  arbitrary = oneof [return MaybeDefined, return Defined]

instance Arbitrary CallProbability where
  arbitrary = oneof [return MaybeWillBeCalled, return WillBeCalled]

instance Arbitrary PositionedFunctionCall where
  arbitrary = do
    size          <- elements [1 .. maxItemsSize]
    callPos       <- choose (1, size)
    functionCalls <- setOf size arbitrary
    return MkPositionedFunctionCall { callPos, functionCalls }

instance Arbitrary FunctionCallWithProbability where
  arbitrary = do
    callProbability <- arbitrary
    functionCall    <- arbitrary
    return MkFunctionCallWithProbability { callProbability, functionCall }

instance Arbitrary UndefinedFunctionProbability where
  arbitrary = do
    size                       <- elements [0 .. maxItemsSize]
    ungroupedCallUndefinedProbabilities <- vectorOf size arbitrary
    resultUndefinedProbability <- arbitrary
    let
      groupedCallUndefinedProbabilities = groupBy
        (\MkPositionedFunctionCall { callPos = callPosLeft } MkPositionedFunctionCall { callPos = callPosRight } ->
          callPosLeft == callPosRight
        )
        ungroupedCallUndefinedProbabilities
    let
      callUndefinedProbabilities = S.fromList $ map
        (foldl1
          (\MkPositionedFunctionCall { callPos = callPos, functionCalls = callsAcc } MkPositionedFunctionCall { functionCalls = currCalls } ->
            MkPositionedFunctionCall { callPos, functionCalls = S.union currCalls callsAcc }
          )
        )
        groupedCallUndefinedProbabilities
    let val = MkUndefinedFunctionProbability { callUndefinedProbabilities, resultUndefinedProbability }
    return MkUndefinedFunctionProbability { callUndefinedProbabilities, resultUndefinedProbability }

instance {-# OVERLAPPING #-} Arbitrary (FunctionSignature,UndefinedFunctionProbability) where
  arbitrary = do
    size                       <- elements [0 .. maxItemsSize]
    signature                  <- arbitrary
    returnProbability          <- arbitrary
    callUndefinedProbabilities <- setOf size arbitrary
    return
      ( signature
      , MkUndefinedFunctionProbability
        { callUndefinedProbabilities
        , resultUndefinedProbability = case fsReturn signature of
          NoReturn -> Nothing
          Return _ -> Just returnProbability
        }
      )
  shrink (a, b) = map (a, ) $ shrink b

instance Arbitrary FunctionDomainConstraint where
  arbitrary = do
    size             <- elements [0 .. maxItemsSize]
    inputConstraints <- vectorOf size arbitrary
    let input = M.fromList inputConstraints
    outputArb <- arbitrary
    let output = Just outputArb
    return MkFunctionDomainConstraint { input, output }


instance Arbitrary FunctionDomainInOutConstraints where
  arbitrary = do
    let from = MkNegInfinite
    let to   = MkPosInfinite
    return MkRangedFunctionDomainInOutConstraint { FDD.from, FDD.to }

instance {-# OVERLAPPING #-} Arbitrary (FunctionSignature, FunctionDomainConstraint) where
  arbitrary = do
    fsName    <- arbitrary
    size      <- elements [0 .. maxItemsSize]
    inputList <- vectorOf size $ do
      p <- arbitrary
      c <- suchThat arbitrary (not . null)
      return (p, c)
    output <- oneof
      [ return Nothing
      , do
        list <- suchThat arbitrary (not . null)
        return $ Just list
      ]
    let input  = M.fromList inputList
    let fsArgs = map MkValue $ zipWith (\n _y -> pack $ "a" ++ show n) [1 .. maxItemsSize * 2] $ M.toList input
    let
      fsReturn = case output of
        Nothing -> NoReturn
        Just _  -> Return "r"
    return (MkFunctionSignature { fsName, fsArgs, fsReturn }, MkFunctionDomainConstraint { input, output })
  shrink (a, b) = map (a, ) $ shrink b

instance {-# OVERLAPPING #-} Arbitrary [(FunctionSignature,UndefinedFunctionProbability)] where
  arbitrary = do
    size <- elements [0 .. maxItemsSize]
    vectorOf size arbitrary

