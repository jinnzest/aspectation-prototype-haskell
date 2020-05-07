module AspectsTestShared
  ( genSemanticTree
  , maxItemsSize
  , extractRight
  ) where

import Data.List (map)
import Data.Map as M (Map, fromList, toList)
import SemanticTree as Sem
  ( SemanticTree(MkSemanticTree)
  , Return(NoReturn)
  , Function(fSignature, fBody, MkFunction)
  , FunctionSignature
  , Statement(MkNoAssignment, expression)
  , Expression(fcsName, fcsArgs, MkFunctionCall, tmpVarName)
  , FunctionHash(MkFunctionHash, statementHashes, functionHash)
  )
import Text.Megaparsec.Pos (SourcePos(SourcePos, sourceName, sourceLine, sourceColumn), mkPos)
import Location as Loc (Range(MkRange, from, to), Ranged(MkRanged, rItem, range))
import Data.Text (pack)
import Data.Maybe (Maybe(Nothing))
import Panic (panic)
import Shared (showT)

maxItemsSize :: Int
maxItemsSize = 3

genSemanticTree :: (a -> Int) -> M.Map FunctionSignature a -> SemanticTree
genSemanticTree findMaxPos analytics =
  let
    sp    = SourcePos { sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1 }
    range = MkRange { Loc.from = sp, Loc.to = sp }
  in MkSemanticTree
    ( fromList
    $ snd
    $ foldl
        (\(c, acc) (f, a) ->
          ( c + 1
          , ( MkFunctionHash { functionHash = showT c, statementHashes = [] }
            , [ MkFunction
                  { fSignature = f
                  , fBody      = map
                    (const MkRanged
                      { rItem = MkNoAssignment
                        { expression = Sem.MkFunctionCall
                          { Sem.fcsName    = MkRanged { rItem = "some_func", range }
                          , Sem.fcsArgs    = []
                          , Sem.tmpVarName = Nothing
                          }
                        }
                      , range
                      }
                    )
                      -- trace ("\n\ncallUndefinedProbabilities a: "++show calls++"\nmaxCallPos: "++show maxCallPos) 
                    [1 .. (findMaxPos a)]
                  }
              ]
            )
            : acc
          )
        )
        (0, [])
    $ toList analytics
    )

extractRight _    (Right r) = r
extractRight body other     = panic ("\nunexpected: " ++ show other ++ "\nbody: " ++ show body)
