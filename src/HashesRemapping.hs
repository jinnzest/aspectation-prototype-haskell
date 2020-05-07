module HashesRemapping
  ( mapOldToNewNames
  , remap
  ) where

import Prelude ()
import SemanticTree as Sem
  ( FunctionHash
  , SemanticModel(tree)
  , _semanticTree
  , Function(MkFunction, fSignature)
  , FunctionSignature(fsName, MkFunctionSignature, fsArgs, fsReturn)
  , Arg(MkValue)
  , Return(NoReturn, Return)
  )
import Data.Map as M (Map, empty, toList, member, insert, fromList, lookup, insertWith)
import Data.Tuple (swap, fst)
import Data.Maybe (Maybe(Nothing), Maybe(Just), fromMaybe)
import Data.Function (($))
import Data.List (map, (++))
import Data.Foldable (Foldable(foldl))
import Data.Eq (Eq((/=)))
import Debug.Trace (trace)
import Text.Show (Show(show))
import Data.Ord (Ord)
import Data.Bifunctor (Bifunctor(bimap))
import FunctionDomainAnalyticsData
  (FunctionDomainAnalytics(MkFunctionDomainAnalytics, _undefinedProbabilities, _constraints))

mapOldToNewNames
  :: Map Sem.FunctionSignature FunctionHash -> SemanticModel -> Map Sem.FunctionSignature [Sem.FunctionSignature]
mapOldToNewNames loadedSigToHashes generatedModel =
  let
    loadedHashMap       = foldl insertSigByHash empty $ toList loadedSigToHashes
    generatedHashToFunc = toList $ _semanticTree $ tree generatedModel
  in foldl (insertNewByOldNames loadedHashMap) empty generatedHashToFunc

insertSigByHash
  :: Map FunctionHash [Sem.FunctionSignature]
  -> (Sem.FunctionSignature, FunctionHash)
  -> Map FunctionHash [Sem.FunctionSignature]
insertSigByHash sigToHashes (sig, hash) = insertWith (++) hash [sig] sigToHashes

insertNewByOldNames loadedHashMap hashMap (hash, functions) =
  let newSignatures = map fSignature functions
  in
    case lookup hash loadedHashMap of
      Nothing      -> hashMap
      Just oldSigs -> foldl (insertOldSig newSignatures) hashMap oldSigs

insertOldSig newSignatures hashMap oldSig = foldl (insertNewSigByOldOnDiff oldSig) hashMap newSignatures

insertNewSigByOldOnDiff oldSig hashMap newSig =
  if oldSig /= newSig then insertWith (++) oldSig [newSig] hashMap else hashMap

remap :: (Eq k, Ord k) => Map k v -> Map k [k] -> Map k v
remap sig mappings =
  fromList $ foldl (\acc (sig, a) -> acc ++ map (, a) (fromMaybe [sig] (lookup sig mappings))) [] $ toList sig


