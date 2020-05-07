module MainParser
  ( parseDir
  ) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, liftIO, mapExceptT, return)
import Data.Text (Text, append, concat, pack, splitOn)
import Data.Text.IO (putStrLn, readFile)
import Prelude (Either(Left, Right), IO, String, (!!), ($), (+), (++), (-), (.), const, length, map, show)
import Text.Megaparsec.Pos (sourceColumn, sourceLine, unPos)

import EmbeddedFuncs (EmbeddedFuncs)
import Errors (Error(MkError, MkRangedError), Errors(MkErrors), errors)
import Location (Range(MkRange), Ranged(MkRanged), from, rItem, range, to)
import SemanticParserIO (parseSemanticIO)
import SemanticTree (SemanticModel)
import SyntaxParserIO (parseSyntaxIO)
import Shared (srcDir, showT)

parseDir :: String -> EmbeddedFuncs -> ExceptT Errors IO SemanticModel
parseDir dir embeddedOnes = do
  liftIO $ putStrLn "\nParsing main language..."
  let filePath = srcDir "main/main.astnm" dir
  parseFile filePath embeddedOnes

parseFile filePath embeddedOnes = do
  fileBody <- liftIO $ readFile filePath
  parse filePath fileBody embeddedOnes

parse filePath fileBody embeddedOnes = do
  tree <- parseSyntaxIO filePath fileBody
  mapResult fileBody $ parseSemanticIO embeddedOnes tree

mapResult src = mapExceptT
  (\result -> do
    res <- result
    return $ case res of
      Left  err -> Left $ mapErrors src err
      Right r   -> Right r
  )

-- extract to handling errors
mapErrors src MkErrors { errors = err } = MkErrors { errors = map (mapError src) err }

mapError :: Text -> Error -> Error
mapError src (MkRangedError MkRanged { rItem = text, range = itemRange }) = MkError $ concat
  [ "error: "
  , text
  , "\n  --> "
  , showT (unPos $ sourceLine $ from itemRange)
  , ":"
  , showT (unPos $ sourceColumn $ from itemRange)
  , "\n\n"
  , extractContext src itemRange
  ]
mapError _ another = another

extractContext src MkRange { from = from, to = to } =
  let
    col           = unPos . sourceColumn
    currLine      = unPos (sourceLine from)
    colFrom       = col from - 2
    colTo         = col to - 3
    splittedInput = splitOn "\n" src
    currLineStr   = show currLine
    lineNumSpace  = append (pack $ map (const ' ') [0 .. (length currLineStr)]) " | "
    extractLine pos = (splittedInput !! (currLine + pos))
    lineNear pos = concat [lineNumSpace, extractLine pos, "\n"]
    currLineFormatted = concat [pack currLineStr, "  | ", extractLine (-1), "\n"]
    underLine =
      concat [lineNumSpace, pack $ map (const ' ') [0 .. colFrom], pack $ map (const '^') [colFrom .. colTo], "\n"]
  in concat [lineNear (-3), lineNear (-2), currLineFormatted, underLine, lineNear 1, lineNear 2]
