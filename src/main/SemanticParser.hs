module SemanticParser
  ( parseSemantic
  ) where

import Control.Monad.Except (ExceptT, Except, foldM, throwError, unless, when, mapExceptT)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 as B (pack, unpack)
import Data.Functor.Identity (Identity)
import Data.Map as M (Map, empty, fromList, insert, insertWith, lookup, member, toList)
import Data.Set as S (Set, empty, fromList, insert, member)
import Data.List (elemIndex, find, init, last)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Text as T (Text, pack, unpack, concat)
import LLVM.AST.Operand (Operand)
import Panic (panic)
import Prelude
  ( Bool(False, True)
  , Int
  , String
  , ($)
  , (+)
  , (++)
  , (.)
  , (/=)
  , (==)
  , concatMap
  , const
  , filter
  , foldl
  , fst
  , head
  , last
  , length
  , map
  , mapM
  , mapM_
  , not
  , null
  , return
  , reverse
  , sequence
  , show
  , snd
  , tail
  , zip
  )

import Debug.Trace (trace)
import EmbeddedFuncs (EmbeddedFuncs, printF)
import Errors (Error(MkError, MkMultiRangedError, MkRangedError), Errors(MkErrors))
import Location
  ( MultiRanged(MkMultiRanged, mrItem, ranges)
  , Positioned(MkPositioned)
  , Range(MkRange, from, to)
  , Ranged(MkRanged, rItem, range)
  )
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , NameArgs(MkNameArgs, naArgs, naName)
  , Return(NoReturn, Return)
  , FunctionHash(MkFunctionHash, functionHash, statementHashes)
  , SemanticModel(MkSemanticModel, tree)
  , SemanticTree(MkSemanticTree)
  , Statement(MkNoAssignment, expression)
  , ExpressionHash(MkExpressionHash, expressionHash, semanticPlace)
  , SemanticPlace(MkSemanticPlace, statementPlace, expressionPlace)
  , tmpVarName
  )
import SyntaxTree as Syn
  ( Expression(MkIdentifier, MkInteger)
  , Function(MkFunction, fBody, fSignature)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName)
  , Statement(MkNoAssignment)
  , SyntaxTree
  )
import Shared (justLookup, showT, genNextIdentifier)
import Data.List as L (concat)
import GHC.Generics (Generic)
import Control.Monad.State.Lazy (State, mapM_, runState, MonadState(get), modify, mapM)
import StateErrors (unpackState)
import Control.Applicative ((<$>))
import Control.Monad.State (gets)

newtype Context = MkContext
  { generatedIdentifiers :: S.Set Text
  }
  deriving Generic

parseSemantic :: EmbeddedFuncs -> SyntaxTree -> ExceptT Errors Identity SemanticModel
parseSemantic embeddedFuncs syntaxTree = mdo
  deduplicatedFunctions <- failWhenDuplicatedFunctions syntaxTree
  let sourceCodeFunctionSignatures = map functionNameArgs deduplicatedFunctions
  nameArgsWithStatements <- joinNameArgsWithStatements
    sourceCodeFunctionSignatures
    syntaxTree
    embeddedFuncs
    functionReturning
  let functionReturning = mkFunctionReturning embeddedFuncs nameArgsWithStatements
  let semanticFunctions = mkSemanticFunctions functionReturning nameArgsWithStatements
  let nameArgsMap       = mkNameArgsMap semanticFunctions
  let hashedFunctions   = mkFunctionHashes semanticFunctions
  let semanticTree      = mkSemanticTree nameArgsMap hashedFunctions
  checkMainPresent nameArgsWithStatements
  checkFunctionReturnForFunctions semanticFunctions
  return $ MkSemanticModel { tree = MkSemanticTree semanticTree }

mkNameArgsMap :: [Sem.Function] -> M.Map NameArgs Sem.Function
mkNameArgsMap = M.fromList . map (\f -> (extractNameArgs f, f))

mkSemanticFunctions :: M.Map NameArgs Return -> [Ranged (Sem.NameArgs, [Ranged Sem.Statement])] -> [Sem.Function]
mkSemanticFunctions functionReturning = map (mkSemanticFunction functionReturning)

mkSemanticTree :: Map NameArgs Sem.Function -> [(NameArgs, FunctionHash)] -> M.Map FunctionHash [Sem.Function]
mkSemanticTree nameArgsMap hashedFunctions =
  mergeTheSameHashFunctions $ joinHashWithFunction nameArgsMap hashedFunctions

joinHashWithFunction :: Map NameArgs Sem.Function -> [(NameArgs, FunctionHash)] -> [(FunctionHash, [Sem.Function])]
joinHashWithFunction nameArgsMap = map (\(na, h) -> (h, [justLookup na nameArgsMap]))

-- TODO consider what to do when two functions has the same hash but different semantics (statements of the functions have different hashes)
mergeTheSameHashFunctions :: [(FunctionHash, [Sem.Function])] -> M.Map FunctionHash [Sem.Function]
mergeTheSameHashFunctions = foldl (\acc (k, v) -> let f new old = new ++ old in insertWith f k v acc) M.empty

checkFunctionReturnForFunctions :: [Sem.Function] -> ExceptT Errors Identity ()
checkFunctionReturnForFunctions functions = mdo
  let bodies = concatMap (\Sem.MkFunction { Sem.fBody = body } -> body) functions
  checkFunctionReturnForStatements bodies

checkFunctionReturnForStatements :: [Ranged Sem.Statement] -> ExceptT Errors Identity ()
checkFunctionReturnForStatements = mapM_ checkFunctionReturnForStatement

checkFunctionReturnForStatement :: Ranged Sem.Statement -> ExceptT Errors Identity ()
checkFunctionReturnForStatement MkRanged { rItem = Sem.MkNoAssignment { expression = expression } } =
  checkFunctionReturnForExpression expression

checkFunctionReturnForExpression :: Sem.Expression -> ExceptT Errors Identity ()
checkFunctionReturnForExpression MkFunctionCall { fcsName = name, fcsArgs = expressions } = do
  mapM_ checkFunctionReturnForSubCall expressions
checkFunctionReturnForExpression _ = return ()

checkFunctionReturnForSubCall :: Sem.Expression -> ExceptT Errors Identity ()
checkFunctionReturnForSubCall MkFunctionCall { fcsName = MkRanged { rItem = name, range = range }, fcsArgs = expressions, tmpVarName = tvn }
  = do
    mapM_ checkFunctionReturnForSubCall expressions
    case tvn of
      Nothing -> throwError $ MkErrors
        [MkRangedError MkRanged { range, rItem = T.concat ["value expected but call of '", name, "' returns nothing"] }]
      Just _ -> return ()
checkFunctionReturnForSubCall _ = return ()

mkFunctionHashes :: [Sem.Function] -> [(Sem.NameArgs, FunctionHash)]
mkFunctionHashes hashedFunctions =
  let
    hashesMap  = M.fromList hashesList
    hashesList = map (\f -> (extractNameArgs f, mkFunctionHash hashesMap f)) hashedFunctions
  in hashesList

mkFunctionHash :: M.Map NameArgs FunctionHash -> Sem.Function -> FunctionHash
mkFunctionHash hashes function =
  let
    statements        = Sem.fBody function
    len               = length statements
    statementsWithPos = zip [1 .. len + 1] $ map rItem statements
    statementHashes   = map mkStatementHash statementsWithPos
    statementsStringWithHashes =
      foldl (\acc (p, s) -> T.concat [acc, " ", mkStatementHashPart hashes function p s]) "" statementsWithPos
  in MkFunctionHash { functionHash = mkHashFromText statementsStringWithHashes, statementHashes }

mkHashFromText :: Text -> Text
mkHashFromText = mkHashFromString . T.unpack

mkHashFromString :: String -> Text
mkHashFromString = T.pack . B.unpack . (encode . hash) . B.pack

extractNameArgs function =
  let sig = Sem.fSignature function in Sem.MkNameArgs { naName = Sem.fsName sig, naArgs = Sem.fsArgs sig }

mkStatementHash :: (Int, Sem.Statement) -> [ExpressionHash]
mkStatementHash (p, Sem.MkNoAssignment { expression = expr }) = mkExpressionHash (p, expr) 1

mkExpressionHash :: (Int, Sem.Expression) -> Int -> [ExpressionHash]

mkExpressionHash (statementPlace, MkConstantInteger _ MkRanged { rItem = item }) expressionPlace =
  [ MkExpressionHash
      { expressionHash = mkHashFromText $ T.concat [showT statementPlace, "_", showT item]
      , semanticPlace  = MkSemanticPlace { statementPlace, expressionPlace }
      }
  ]

mkExpressionHash (statementPlace, MkFunctionArgument _ MkRanged { rItem = item }) expressionPlace =
  [ MkExpressionHash
      { expressionHash = mkHashFromText $ T.concat [showT statementPlace, "_", showT item]
      , semanticPlace  = MkSemanticPlace { statementPlace, expressionPlace }
      }
  ]

mkExpressionHash (statementPlace, MkFunctionCall { fcsName = MkRanged { rItem = name }, fcsArgs = args }) expressionPlace
  = let
      argsHashes =
        concatMap (\s -> map expressionHash $ mkExpressionHash (statementPlace, s) (expressionPlace + 1)) args
    in
      MkExpressionHash
          { expressionHash = mkHashFromText $ T.concat [showT statementPlace, "_", showT expressionPlace, name]
          , semanticPlace  = MkSemanticPlace { statementPlace, expressionPlace }
          }
        : map
            (\h -> MkExpressionHash
              { expressionHash = mkHashFromText $ T.concat [showT statementPlace, "_", showT expressionPlace, h]
              , semanticPlace  = MkSemanticPlace { statementPlace, expressionPlace }
              }
            )
            argsHashes

mkStatementHashPart :: M.Map NameArgs FunctionHash -> Sem.Function -> Int -> Sem.Statement -> Text
mkStatementHashPart hashes nameArgs place statement =
  T.concat [showT place, "_", mkStatementHashPartSource hashes nameArgs statement]

mkStatementHashPartSource :: M.Map NameArgs FunctionHash -> Sem.Function -> Sem.Statement -> Text
mkStatementHashPartSource _ _ Sem.MkNoAssignment { expression = MkConstantInteger _ MkRanged { rItem = constInt } } =
  showT constInt
mkStatementHashPartSource _ _ Sem.MkNoAssignment { expression = MkFunctionArgument _ MkRanged { rItem = argPos } } =
  T.pack $ "arg_" ++ show argPos
mkStatementHashPartSource hashes nameArgs Sem.MkNoAssignment { expression = Sem.MkFunctionCall { fcsName = naName, fcsArgs = args } }
  = let maybeFound = M.lookup (MkNameArgs { naName = rItem naName, naArgs = map (const $ MkValue "v") args }) hashes
    in maybe "abstract" functionHash maybeFound

mkSemanticFunction :: M.Map NameArgs Return -> Ranged (Sem.NameArgs, [Ranged Sem.Statement]) -> Sem.Function
mkSemanticFunction functionReturning MkRanged { rItem = (sig, fBody) } = Sem.MkFunction
  { Sem.fSignature = Sem.MkFunctionSignature
    { Sem.fsName = naName sig
    , Sem.fsArgs = naArgs sig
    , fsReturn   = justLookup MkNameArgs { naName = naName sig, naArgs = naArgs sig } functionReturning
    }
  , Sem.fBody
  }

mkFunctionReturning :: EmbeddedFuncs -> [Ranged (Sem.NameArgs, [Ranged Sem.Statement])] -> M.Map NameArgs Return
mkFunctionReturning embeddedFuncs semanticFunctions =
  let
    print = printF embeddedFuncs
    pairs =
      (signatureNameArgs print, fsReturn print)
        : map
            (\MkRanged { rItem = (fs, ss) } ->
              ( MkNameArgs { naName = naName fs, naArgs = naArgs fs }
              , returnKind returnMap fs $ expression $ rItem $ last ss
              )
            )
            semanticFunctions
    returnMap = M.fromList pairs
  in returnMap

returnKind :: M.Map NameArgs Return -> NameArgs -> Sem.Expression -> Return
returnKind _ nameArgs (MkConstantInteger  _ _) = generateReturnIndentifier nameArgs
returnKind _ nameArgs (MkFunctionArgument _ _) = generateReturnIndentifier nameArgs
returnKind returnMap _ MkFunctionCall { fcsName = naName, fcsArgs = args } =
  justLookup MkNameArgs { naName = rItem naName, naArgs = map (const $ MkValue "v") args } returnMap

generateReturnIndentifier :: NameArgs -> Return
generateReturnIndentifier MkNameArgs { naName = naName, naArgs = naArgs } =
  let generatedIdentifiers = S.fromList $ naName : map extractArgName naArgs
  in if S.member "r" generatedIdentifiers then Return $ genNextIdentifier generatedIdentifiers else Return "r"

extractArgName v = let MkValue t = v in t

mkFunctionOverloading :: EmbeddedFuncs -> [Sem.Function] -> M.Map Sem.NameArgs Sem.Function
mkFunctionOverloading embeddedFuncs sourceFunctions =
  let
    printSig = printF embeddedFuncs
    print    = (Sem.fsName printSig, Sem.fsArgs printSig)
  in M.fromList $ map (\f -> (signatureToNameArgs $ Sem.fSignature f, f)) sourceFunctions

failWhenDuplicatedFunctions :: [Ranged Syn.Function] -> ExceptT Errors Identity [Ranged Syn.Function]
failWhenDuplicatedFunctions functions = do
  let
    rangedNames = map
      (\MkRanged { rItem = Syn.MkFunction { Syn.fSignature = MkRanged { rItem = Syn.MkFunctionSignature { Syn.fsName = MkRanged { rItem = name } } } }, range = range } ->
        MkRanged { rItem = name, range }
      )
      functions
  checkNameDuplicates rangedNames
  return functions

checkNameDuplicates :: [Ranged Text] -> ExceptT Errors Identity (M.Map Text Range)
checkNameDuplicates = foldM
  (\acc rv@MkRanged { rItem = name, range = r } -> case M.lookup name acc of
    Just lastDefinition -> throwError $ MkErrors
      [ MkMultiRangedError
          $ MkMultiRanged { mrItem = T.concat ["Duplicated function '", name, "'"], ranges = [r, lastDefinition] }
      ]
    Nothing -> return $ M.insert name r acc
  )
  M.empty

argsLength []          = "0 arguments"
argsLength [MkValue _] = "1 argument"
argsLength args        = T.concat [showT $ length args, " arguments"]

checkMainPresent :: [Ranged (Sem.NameArgs, [Ranged Sem.Statement])] -> ExceptT Errors Identity ()
checkMainPresent semanticFunctions = do
  let mainFuncList = filter (\f -> naName (fst $ rItem f) == "main") semanticFunctions
  when (null mainFuncList) $ throwError $ MkErrors [MkError "main function is missing"]

joinNameArgsWithStatements
  :: [NameArgs]
  -> SyntaxTree
  -> EmbeddedFuncs
  -> M.Map NameArgs Return
  -> ExceptT Errors Identity [Ranged (Sem.NameArgs, [Ranged Sem.Statement])]
joinNameArgsWithStatements sourceCodeFunctionSignatures syntaxTree embeddedFuncs functionReturning = do
  let
    functionSignatures =
      M.fromList
        $ map (\fs -> (naName fs, naArgs fs))
        $ signatureNameArgs (printF embeddedFuncs)
        : sourceCodeFunctionSignatures
  mapM (semanticFunctionItems functionSignatures functionReturning) syntaxTree

signatureNameArgs :: Sem.FunctionSignature -> NameArgs
signatureNameArgs f = MkNameArgs { naName = Sem.fsName f, naArgs = Sem.fsArgs f }

functionNameArgs :: Ranged Syn.Function -> NameArgs
functionNameArgs f =
  let
    sig    = rItem $ Syn.fSignature $rItem f
    naName = rItem $ Syn.fsName sig
    naArgs = map (semanticArg . rItem) $ Syn.fsArgs sig
  in MkNameArgs { naName, naArgs }

semanticArg = MkValue

semanticFunctionItems
  :: Map Text [Arg]
  -> M.Map NameArgs Return
  -> Ranged Syn.Function
  -> ExceptT Errors Identity (Ranged (Sem.NameArgs, [Ranged Sem.Statement]))
semanticFunctionItems signatures functionReturning f = do
  let
    syntaxSignature      = rItem $ Syn.fSignature $ rItem f
    naName               = rItem $ Syn.fsName syntaxSignature
    argNames             = map rItem $ Syn.fsArgs syntaxSignature
    naArgs               = map semanticArg argNames
    nameArgs             = MkNameArgs { naName, naArgs }
    generatedIdentifiers = S.fromList $ naName : argNames
  let mStatements = map (semanticStatement signatures argNames functionReturning) (Syn.fBody (rItem f))
  body <- mapExceptT (unpackState MkContext { generatedIdentifiers }) $ sequence mStatements
  let bodyWithRemovedUnusedTmpVarNames = map removeTmpVarNameFromStatement (init body) ++ [last body]
  return MkRanged { rItem = (nameArgs, bodyWithRemovedUnusedTmpVarNames), range = range f }

semanticStatement
  :: Map Text [Arg]
  -> [Text]
  -> M.Map NameArgs Return
  -> Ranged Syn.Statement
  -> ExceptT Errors (State Context) (Ranged Sem.Statement)
semanticStatement signatures args functionReturning statement = do
  let MkRanged { rItem = Syn.MkNoAssignment syntaxExpressions, range = r } = statement
  expressions <- semanticExpression signatures args syntaxExpressions functionReturning
  let semanticExpressions   = snd expressions
  let syntaxExpressionsLeft = fst expressions
  unless (null syntaxExpressionsLeft) $ throwError $ MkErrors
    [mkUnexpectedExpressionError semanticExpressions syntaxExpressionsLeft]
  return $ MkRanged { rItem = Sem.MkNoAssignment $ rItem semanticExpressions, range = r }

removeTmpVarNameFromStatement :: Ranged Sem.Statement -> Ranged Sem.Statement
removeTmpVarNameFromStatement MkRanged { range = r, rItem = Sem.MkNoAssignment { expression = expr } } =
  MkRanged { rItem = Sem.MkNoAssignment { expression = removeTmpVarNameFromExpr expr }, range = r }

removeTmpVarNameFromExpr :: Sem.Expression -> Sem.Expression
removeTmpVarNameFromExpr MkFunctionCall { fcsName = fn, fcsArgs = fa } =
  MkFunctionCall { fcsName = fn, fcsArgs = fa, tmpVarName = Nothing }
removeTmpVarNameFromExpr (MkConstantInteger  _ c) = MkConstantInteger Nothing c
removeTmpVarNameFromExpr (MkFunctionArgument _ a) = MkFunctionArgument Nothing a

mkUnexpectedExpressionError MkRanged { rItem = MkFunctionCall { fcsName = fcsName, fcsArgs = fcsArgs } } syntaxExpression
  = let nameRange = range fcsName
    in
      MkRangedError $ MkRanged
        { rItem = T.concat
          [ showT (length fcsArgs + 1)
          , " args but function '"
          , rItem fcsName
          , "' expected "
          , showT (length fcsArgs)
          , " args"
          ]
        , range = nameRange
        }
mkUnexpectedExpressionError MkRanged { rItem = MkConstantInteger _ text } syntaxExpression =
  let unexpectedExpressionRange = range $ head syntaxExpression
  in
    MkRangedError $ MkRanged
      { rItem = T.pack $ "Unexpected expression: '" ++ filter (/= '"') (show $ rItem $ head syntaxExpression) ++ "'"
      , range = unexpectedExpressionRange
      }
mkUnexpectedExpressionError unexpected syntaxExpression =
  panic ("Internal compiler error = " ++ show unexpected ++ " :  " ++ show syntaxExpression)

semanticExpression
  :: Map Text [Arg]
  -> [Text]
  -> [Ranged Syn.Expression]
  -> M.Map NameArgs Return
  -> ExceptT Errors (State Context) ([Ranged Syn.Expression], Ranged Sem.Expression)
semanticExpression signatures args syntaxExpressions functionReturning = do
  nextId <- genNextIdentifierCtx
  let
    expression      = head syntaxExpressions
    expressionsLeft = tail syntaxExpressions
  case expression of
    MkRanged { rItem = MkInteger integer, range = range } -> return
      ( expressionsLeft
      , MkRanged { rItem = MkConstantInteger (Just nextId) MkRanged { rItem = integer, range = range }, range }
      )
    MkRanged { rItem = MkIdentifier ident, range = range } ->
      identifier MkRanged { rItem = ident, range } expressionsLeft args signatures functionReturning

identifier
  :: Ranged Text
  -> [Ranged Syn.Expression]
  -> [Text]
  -> Map Text [Arg]
  -> M.Map NameArgs Return
  -> ExceptT Errors (State Context) ([Ranged Syn.Expression], Ranged Sem.Expression)
identifier MkRanged { rItem = expression, range = range } expressionsLeft args signatures functionReturning = do
  nextId <- genNextIdentifierCtx
  case elemIndex expression args of
    Just pos -> case expressionsLeft of
      [] -> return
        ( expressionsLeft
        , MkRanged { rItem = MkFunctionArgument (Just nextId) MkRanged { rItem = pos, range = range }, range = range }
        )
      (maybeName : _) -> case M.lookup expression signatures of
        Nothing -> return
          ( expressionsLeft
          , MkRanged { rItem = MkFunctionArgument (Just nextId) MkRanged { rItem = pos, range = range }, range = range }
          )
        Just toCallFuncArgs -> functionCall
          MkRanged { rItem = (expression, toCallFuncArgs), range }
          signatures
          args
          ((MkRanged { rItem = Syn.MkIdentifier expression, range = range }) : expressionsLeft)
          functionReturning
    Nothing -> case M.lookup expression signatures of
      Nothing -> throwError $ MkErrors
        [MkRangedError $ MkRanged { rItem = T.concat ["The '", expression, "' is undefined identifier"], range }]
      Just toCallFuncArgs -> functionCall
        MkRanged { rItem = (expression, toCallFuncArgs), range }
        signatures
        args
        expressionsLeft
        functionReturning

functionCall
  :: Ranged (Text, [Arg])
  -> Map Text [Arg]
  -> [Text]
  -> [Ranged Syn.Expression]
  -> M.Map NameArgs Return
  -> ExceptT Errors (State Context) ([Ranged Syn.Expression], Ranged Sem.Expression)
functionCall MkRanged { rItem = (fcsName, args), range = range } signatures parentFuncArgs initialExpressionsLeft functionReturning
  = do
    nextId                        <- genNextIdentifierCtx
    (expressionsLeft, revfcsArgs) <- foldM
      (\(synExpr, semExpr) _ -> do
        (synE, semE) <- do
          when (null synExpr) $ throwError $ MkErrors
            [ MkRangedError $ MkRanged
                { rItem = T.concat ["Expected one more argument to the function call '", fcsName, "'"]
                , range = range
                }
            ]
          semanticExpression signatures parentFuncArgs synExpr functionReturning
        return (synE, semE : semExpr)
      )
      (initialExpressionsLeft, [])
      args
    let fcsArgs = map rItem $ reverse revfcsArgs
    let
      fcsReturn =
        justLookup MkNameArgs { naName = fcsName, naArgs = map (const $ MkValue "v") fcsArgs } functionReturning
    let tmpVarName = if fcsReturn == NoReturn then Nothing else Just nextId
    return
      ( expressionsLeft
      , MkRanged
        { rItem = MkFunctionCall { fcsName = MkRanged { rItem = fcsName, range }, fcsArgs, tmpVarName }
        , range
        }
      )

ranged f MkRanged { rItem = i, range = range } = MkRanged { rItem = f i, range }

signatureToNameArgs Sem.MkFunctionSignature { Sem.fsName = naName, Sem.fsArgs = naArgs } =
  MkNameArgs { naName, naArgs }

genNextIdentifierCtx :: ExceptT Errors (State Context) Text
genNextIdentifierCtx = gets (genNextIdentifier . generatedIdentifiers)
