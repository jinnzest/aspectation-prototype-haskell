module AspectsTestArbitrary
  () where

import IdentifierTestArbitrary
import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, testProperty, suchThat, oneof, choose)
import SemanticTree
  (FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn), Return(Return, NoReturn), Arg(MkValue))
import AspectsData (FunctionCall(MkFunctionCall, fcName, fcArgs, tmpVarName), Directives)
import Data.Text (Text, pack)
import AspectsTestShared (maxItemsSize)
import Data.Set (toList, fromList)
import qualified Data.Text as T

instance Arbitrary FunctionCall where
  arbitrary = do
    size         <- elements [0 .. maxItemsSize]
    fcName       <- arbitrary
    fcArgs       <- vectorOf size arbitrary
    isTmpVarName <- arbitrary
    tmpName      <- elements ['a' .. 'z']
    let tmpVarName = if isTmpVarName then Just (T.pack $ tmpName : "") else Nothing
    return MkFunctionCall { fcName, fcArgs, tmpVarName }
