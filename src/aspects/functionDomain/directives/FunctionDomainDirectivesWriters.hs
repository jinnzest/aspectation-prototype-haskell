module FunctionDomainDirectivesWriters
  ( writeFunctionDomainDirectivesMap
  , writeFunctionDomainSigConstraint
  ) where

import Prelude (Integer)
import Data.Int (Int)
import Data.List (map, reverse, sort)
import Data.Function (($), (.))
import Data.Tuple (uncurry)
import Data.Bool (not)
import Data.Ord (Ord(compare))
import Text.Show (Show(show))
import AspectWriters (writeFunctionSignature)
import Data.Map as M (Map, empty, fromList, toList, lookup)
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Sort (sortBy)
import Data.Text as T (Text, append, concat, empty, intercalate, length, null, pack, unpack)
import Debug.Trace ()
import Data.List as L (null, filter)
import FunctionDomainDirectivesData
  ( FunctionDomainDirectives
  , FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , from
  , input
  , output
  , to
  , FunctionDomainConstraintTo(MkPosInfinite, MkIntegerTo)
  , FunctionDomainConstraintFrom(MkNegInfinite, MkIntegerFrom)
  )
import Data.List.Index (indexed)
import SemanticTree (FunctionSignature(fsArgs), showArg, unpackArg)
import Shared (showT)

writeFunctionDomainDirectivesMap :: FunctionDomainDirectives -> Text
writeFunctionDomainDirectivesMap analytics =
  intercalate "\n" $ sort $ map (uncurry writeDirectivesItem) $ toList analytics

writeDirectivesItem :: FunctionSignature -> FunctionDomainConstraint -> Text
writeDirectivesItem sig v = concat [writeFunctionSignature sig, " ~ ", writeFunctionDomainSigConstraint sig v]

writeFunctionDomainSigConstraint :: FunctionSignature -> FunctionDomainConstraint -> Text
writeFunctionDomainSigConstraint sig v =
  let
    constaints = input v
    inputPart =
      if L.null constaints then "" else writeFunctionDomainInConstraint (map unpackArg $ fsArgs sig) constaints
    outputPart = maybe "" (writeFunctionDomainOutConstraint "r") $ output v
  in intercalate ", " $ filter (not . T.null) [inputPart, outputPart]

writeFunctionDomainInConstraint :: [Text] -> M.Map Int [FunctionDomainInOutConstraints] -> Text
writeFunctionDomainInConstraint []   _           = ""
writeFunctionDomainInConstraint args constraints = if L.null constraints
  then ""
  else
    intercalate ", "
    $ map (uncurry writeFunctionDomainContraintOr)
    $ filter (\(_, c) -> not $ L.null c)
    $ map (\(i, a) -> (a, fromMaybe [] (M.lookup i constraints)))
    $ indexed args

sortByArgs :: [(Text, [FunctionDomainInOutConstraints])] -> [(Text, [FunctionDomainInOutConstraints])]
sortByArgs = sortBy (\(a1, _) (a2, _) -> compare a1 a2)

writeFunctionDomainOutConstraint :: Text -> [FunctionDomainInOutConstraints] -> Text
writeFunctionDomainOutConstraint = writeFunctionDomainContraintOr

writeFunctionDomainContraintOr :: Text -> [FunctionDomainInOutConstraints] -> Text
writeFunctionDomainContraintOr var constraints =
  intercalate " or " $ map (writeFunctionDomainConstraint var) constraints

writeFunctionDomainConstraint :: Text -> FunctionDomainInOutConstraints -> Text
writeFunctionDomainConstraint a MkRangedFunctionDomainInOutConstraint { from = from, to = to } =
  concat [writeValueFrom from, "<=", a, "<=", writeValueTo to]

writeValueTo MkPosInfinite     = "inf"
writeValueTo (MkIntegerTo int) = showT int
writeValueFrom MkNegInfinite       = "-inf"
writeValueFrom (MkIntegerFrom int) = showT int

