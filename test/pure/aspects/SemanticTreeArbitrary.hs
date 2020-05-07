module SemanticTreeArbitrary
  ( maxItemsSize
  , mkRanged
  ) where

import Prelude ()
import IdentifierTestArbitrary
import AspectsTestShared (maxItemsSize)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink), elements, vectorOf, testProperty, suchThat, oneof, choose)
import SemanticTree
  ( SemanticTree(MkSemanticTree, _semanticTree)
  , Function(MkFunction, fSignature, fBody)
  , FunctionSignature(MkFunctionSignature, fsName, fsArgs, fsReturn)
  , Expression(MkFunctionCall, fcsName, fcsArgs, MkFunctionArgument, MkConstantInteger, tmpVarName)
  , Arg(MkValue)
  , Return(NoReturn, Return)
  , Statement(MkNoAssignment)
  , FunctionHash(MkFunctionHash, functionHash, statementHashes)
  , ExpressionHash(MkExpressionHash, expressionHash)
  , SemanticPlace(MkSemanticPlace)
  , ExpressionHash(semanticPlace)
  , SemanticPlace(MkSemanticPlace, statementPlace, expressionPlace)
  )
import Location (Ranged(MkRanged, rItem, range), Range(MkRange, from, to))
import Text.Megaparsec.Pos (SourcePos(SourcePos, sourceName, sourceLine, sourceColumn), mkPos)
import Data.Map as M (fromList)
import Data.Maybe (Maybe(Nothing))
import Data.Text (Text, pack)
import Data.Set as S (toList, fromList)
import Data.List as Lst (take, last, length, map, zip, zipWith, filter)
import Data.Functor ((<$>))
import Data.Function (($), const, (.))
import Data.Eq (Eq((==), (/=)))
import Control.Monad (Monad(return))
import TestShared (mkRange)


mkRanged rItem = MkRanged { rItem, range = mkRange (1, 1) (2, 2) }

instance Arbitrary Arg where
  arbitrary = MkValue <$> arbitrary

instance Arbitrary FunctionSignature where
  arbitrary = do
    size         <- elements [0 .. 4]
    fsName       <- arbitrary
    indentifiers <- vectorOf size arbitrary
    let uniqueIdentifiers = S.toList $ S.fromList indentifiers
    let fsArgs            = Lst.map MkValue $ take 3 uniqueIdentifiers
    let fsReturn = if Lst.length uniqueIdentifiers == 4 then Return (Lst.last uniqueIdentifiers) else NoReturn
    return $ MkFunctionSignature { fsName, fsArgs, fsReturn }

instance Arbitrary Function where
  arbitrary = do
    fSignature <- arbitrary
    size       <- elements [0 .. maxItemsSize]
    let fBody = []
    return $ MkFunction { fSignature, fBody }

instance Arbitrary Statement where
  arbitrary = MkNoAssignment <$> arbitrary

instance Arbitrary Expression where
  arbitrary = MkConstantInteger Nothing . mkRanged <$> arbitrary

instance Arbitrary FunctionHash where
  arbitrary = do
    functionHash    <- arbitrary
    size            <- elements [1 .. 3]
    statementHashes <- vectorOf size $ vectorOf size arbitrary
    return MkFunctionHash { functionHash, statementHashes }

instance Arbitrary ExpressionHash where
  arbitrary = do
    expressionHash  <- arbitrary
    statementPlace  <- arbitrary
    expressionPlace <- arbitrary
    return MkExpressionHash { expressionHash, semanticPlace = MkSemanticPlace { statementPlace, expressionPlace } }

instance Arbitrary SemanticTree where
  arbitrary = do
    size      <- elements [0 .. 3]
    functions <- vectorOf size arbitrary
    hashes    <- vectorOf size arbitrary
    let
      semanticTreeList = zip
        hashes
        [ map
            (\f -> MkFunction
              { fSignature = fSignature f
              , fBody      =
                map
                    (\f2 ->
                      let sig = fSignature f2
                      in
                        mkRanged $ MkNoAssignment $ MkFunctionCall
                          { fcsName    = mkRanged $ fsName sig
                          , fcsArgs    = zipWith
                            (\p a -> MkFunctionArgument Nothing (mkRanged p))
                            [0 .. (length $ fsArgs sig)]
                            (fsArgs sig)
                          , tmpVarName = Nothing
                          }
                    )
                  $ filter (\f3 -> fSignature f3 /= fSignature f) functions
              }
            )
            functions
        ]
    return $ MkSemanticTree $ M.fromList semanticTreeList
