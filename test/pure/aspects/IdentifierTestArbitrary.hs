module IdentifierTestArbitrary where

import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, testProperty, suchThat, oneof, choose)
import Data.Text (Text, pack)
import AspectsTestShared (maxItemsSize)

instance {-# OVERLAPS #-} Arbitrary Text where
  arbitrary = do
    size       <- elements [1 .. maxItemsSize]
    identifier <- vectorOf size $ elements ['a' .. 'z']
    return $ pack identifier
