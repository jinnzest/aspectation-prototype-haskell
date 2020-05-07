{-# LANGUAGE RankNTypes #-}
module Shared
  ( separator
  , second3
  , insertMissing
  , toJSONValue
  , justLookup
  , justGet
  , srcDir
  , showT
  , genNextIdentifier
  ) where


import Prelude ()
import Data.Map as M (Map, member, insert, lookup, toList)
import Data.Aeson (ToJSON, Value(String), encode)
-- import Data.Aeson.QQ ()
import Data.Ord (Ord)
import Data.Text as T (pack, intercalate, Text)
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Panic (panic)
import Data.List ((++), map, concat, concatMap, (!!), init, tail, filter, null, find)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.String (String)
import Data.Eq (Eq((/=)))
import Text.Show (Show(show))
import qualified Data.Set as S
import Data.Bool (not)

separator ""       = ""
separator notEmpty = ","

second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f (a, b, c) = (a, f b, c)

insertMissing :: (Eq k, Ord k) => Map k v -> (k, v) -> Map k v
insertMissing hm (k, v) = if M.member k hm then hm else M.insert k v hm

toJSONValue :: Show a => a -> Value
toJSONValue = String . pack . show

justLookup :: (Show k, Eq k, Ord k, Show v) => k -> Map k v -> v
justLookup k m = case lookup k m of
  Nothing -> panic $ "\nThe key '" ++ show k ++ "' can't be found in the map: \n" ++ concatMap
    (\v -> "\n" ++ show v)
    (M.toList m)
  Just v -> v

justGet :: Show p => [p] -> Int -> p
justGet a p =
  if null a then panic ("\nPosition " ++ show p ++ " is bigger then size of the array " ++ show a ++ "") else a !! p

srcDir :: String -> String -> String
srcDir path dir = dir ++ "/src/" ++ path

showT :: forall a . Show a => a -> Text
showT = T.pack . show

genNextIdentifier :: S.Set Text -> Text
genNextIdentifier generatedIdentifiers = do
  let identifiers = map T.pack $ filter (/= "") $ concatMap (\v -> map (v ++) genFlat) genFlat
  fromJust $ find (\i -> not $ S.member i generatedIdentifiers) identifiers

genFlat :: [String]
genFlat = "" : map (: "") ['a' .. 'z']
