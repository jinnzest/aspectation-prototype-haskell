module FunctionDomainDirectives
  ( FunctionDomainDirectives
  , defaultFunctionDomainDirective
  , propagateFunctionDomainDirectives
  , embeddedFunctionDomainDirectives
  , lessToFrom
  ) where

import Prelude (Integer)
import Text.Show (Show(show))
import Data.Maybe (Maybe, fromJust, isJust, Maybe(Just, Nothing), maybe, maybeToList)
import Data.Int (Int)
import Data.Bool (Bool(False, True), (||), not, (&&), otherwise)
import Data.Function (($), (.), const, id)
import Data.Tuple (snd, fst, swap)
import Data.Ord (Ord((<), (>)))
import Data.Eq (Eq((==)))
import Data.List ((!!))
import Data.Either (Either(Left, Right), either)
import Data.Functor.Identity (Identity)
import Data.Bifunctor (second)
import Control.Monad.Except (Except, ExceptT, throwError, runExcept, runExceptT, mapExceptT)
import Control.Monad (foldM)
import Data.Map as M (Map, empty, fromList, insert, lookup, member, toList, unionWith, insertWith, singleton, union)
import Data.Set as S (Set, empty, fromList, insert, member, toList)
import Data.List as L
  (concat, groupBy, head, null, sortBy, zip, reverse, map, filter, foldl, concatMap, (++), any, last)
import Data.List.Index (indexed)
import Data.Text as T (Text, pack, concat, unpack)
import Data.Text.IO (putStrLn)
import Panic (panic)
import Deriving.Aeson.Stock (Vanilla)
import Deriving.Aeson (Generic, ToJSON, CustomJSON(CustomJSON))
import GHC.Generics (Generic)
import Data.Aeson (Value, ToJSON, toJSON)
import Debug.Trace (trace)
import Data.Aeson.QQ ()
import Control.Monad.State.Lazy (Monad(return, (>>)), mapM, (=<<), State, MonadState(get), runState, modify)
import Data.Traversable (Traversable)

import Errors (Errors(MkErrors), Error(MkError))
import AspectsData as Asp (Directives, EmptyMap)
import FunctionDomainDirectivesData
  ( FunctionDomainConstraint(MkFunctionDomainConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainInOutConstraints(MkRangedFunctionDomainInOutConstraint)
  , FunctionDomainConstraintFrom(MkNegInfinite, MkIntegerFrom)
  , FunctionDomainConstraintTo(MkPosInfinite, MkIntegerTo)
  , from
  , input
  , output
  , to
  , FunctionDomainDirectives
  )
import Location (Ranged(MkRanged, rItem, range))
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName, tmpVarName)
  , FunctionSignature(MkFunctionSignature, fsArgs, fsName, fsReturn)
  , Return(NoReturn, Return)
  , SemanticTree(_semanticTree)
  , Statement(MkNoAssignment)
  , expression
  , fBody
  , fSignature
  , showArg
  , unpackArg
  , Function(MkFunction)
  , tmpVarToReturn
  )
import Shared (separator, second3, insertMissing, justLookup, showT)
import ShowJ (showJ)
import StateErrors (unpackState)

defaultFunctionDomainDirective :: Function -> FunctionDomainConstraint
defaultFunctionDomainDirective MkFunction { fSignature = MkFunctionSignature { fsArgs = args, fsReturn = ret } } =
  MkFunctionDomainConstraint
    { input  = M.fromList $ map (\(a, _) -> (a, defaultConstraint)) $ indexed args
    , output = case ret of
      Return _ -> Just defaultConstraint
      NoReturn -> Nothing
    }

defaultConstraint = [MkRangedFunctionDomainInOutConstraint { from = MkNegInfinite, to = MkPosInfinite }]

data PropagatingContext = MkPropagatingContext
  { functions           :: Map FunctionSignature [Statement]
  , propagatedFunctions :: S.Set FunctionSignature
  , inputDirectives     :: Directives (M.Map Int [FunctionDomainInOutConstraints])
  , outputDirectives    :: Directives [FunctionDomainInOutConstraints]
  , callers             :: S.Set FunctionSignature
  }
  deriving Generic
  deriving ToJSON via Vanilla PropagatingContext

instance Show PropagatingContext where
  show = showJ

embeddedFunctionDomainDirectives :: FunctionDomainDirectives
embeddedFunctionDomainDirectives = M.fromList
  [ ( MkFunctionSignature { fsName = "print", fsArgs = [MkValue "v"], fsReturn = NoReturn }
    , MkFunctionDomainConstraint { input = M.fromList [(0, defaultConstraint)], output = Nothing }
    )
  ]

propagateFunctionDomainDirectives :: FunctionDomainDirectives -> SemanticTree -> Except Errors FunctionDomainDirectives
propagateFunctionDomainDirectives initialDirectives tree = do
  let functionsList       = extractFunctions tree
  let functions           = M.fromList functionsList
  let propagatedFunctions = S.empty
  let callers             = S.empty
  let inputDirectives = extractInputConstraints initialDirectives
  let outputDirectives = extractOutputConstraints initialDirectives
  let
    initialContext =
      MkPropagatingContext { functions, propagatedFunctions, inputDirectives, outputDirectives, callers }
  propagatedFunctions <- mapExceptT (unpackState initialContext) $ mapM propagateFunctionBody functionsList
  let
    propagatedDirectives = -- trace ("\npropagatedFunctions: "++show propagatedFunctions)
      M.fromList
        $ map (\(s, input, output) -> (s, MkFunctionDomainConstraint { input, output }))
        $ map (second3 M.fromList) $filter anyDefined
        $ map (second3 M.toList) propagatedFunctions
  let res = mergeDirectives propagatedDirectives initialDirectives
  return -- $ trace ("\nres: "++show res) 
         res

mergeDirectives :: FunctionDomainDirectives -> FunctionDomainDirectives -> FunctionDomainDirectives
mergeDirectives propagatedDirectives = foldl insertMissing propagatedDirectives . M.toList

extractFunctions :: SemanticTree -> [(FunctionSignature, [Statement])]
extractFunctions = map (\f -> (fSignature f, map rItem $ fBody f)) . concatMap snd . M.toList . _semanticTree

extractInputConstraints :: FunctionDomainDirectives -> Map FunctionSignature (Map Int [FunctionDomainInOutConstraints])
extractInputConstraints = M.fromList . map (second input) . M.toList

extractOutputConstraints :: FunctionDomainDirectives -> Map FunctionSignature [FunctionDomainInOutConstraints]
extractOutputConstraints initialDirectives =
  M.fromList
    $ map (\(s, h) -> (s, fromJust $ output h))
    $ filter
        (\(_, h) -> case output h of
          Nothing -> False
          Just _  -> True
        )
    $ M.toList initialDirectives

anyDefined :: (a, [(Int, a2)], Maybe a2) -> Bool
anyDefined (_, _ : _, _     ) = True
anyDefined (_, _    , Just _) = True
anyDefined (_, _    , _     ) = False

propagateFunctionBody
  :: (FunctionSignature, [Statement])
  -> ExceptT
       Errors
       (State PropagatingContext)
       (FunctionSignature, M.Map Int [FunctionDomainInOutConstraints], Maybe [FunctionDomainInOutConstraints])
propagateFunctionBody function@(signature, statements) = do
  modify
    (\s -> MkPropagatingContext
      { functions           = functions s
      , propagatedFunctions = propagatedFunctions s
      , inputDirectives     = inputDirectives s
      , outputDirectives    = outputDirectives s
      , callers             = S.insert signature $ callers s
      }
    )
  maybeInput      <- propagateFunctionBodyInput function
  maybeOutput     <- propagateFunctionBodyOutput function
  (input, output) <- inputOutputIntersection (signature, L.reverse statements) (M.toList maybeInput) maybeOutput
  modify
    (\s -> MkPropagatingContext
      { functions           = functions s
      , propagatedFunctions = propagatedFunctions s
      , inputDirectives     = if null $ M.toList input
        then inputDirectives s
        else M.insert signature input $ inputDirectives s
      , outputDirectives    = case output of
        Nothing         -> outputDirectives s
        Just directives -> M.insert signature directives $ outputDirectives s
      , callers             = callers s
      }
    )
  return (signature, input, output)

propagateFunctionBodyInput
  :: (FunctionSignature, [Statement])
  -> ExceptT Errors (State PropagatingContext) (M.Map Int [FunctionDomainInOutConstraints])
propagateFunctionBodyInput (signature, statements) = do
  context     <- get
  constraints <- mapM propagateStatement statements
  let groupedConstraints  = foldl (unionWith (++)) M.empty constraints
  let stictestConstraints = map strictestConstraintGroupsPerArg $ M.toList groupedConstraints
  -- res <-
  if null stictestConstraints || existsNotNullConstraint stictestConstraints
    then intersectPropagatedAndInitialConstraints signature stictestConstraints
    else throwFunctionIsUndefinedForDomainIntersectionOftheInputDirectivesOfSubFunctions signature
  -- return  -- $ trace ("\npropagateFunctionBodyInput, signature: "++show signature++", statements: "++show statements++"\nconstraints: "++show constraints++", res: "++show res) res


intersectPropagatedAndInitialConstraints
  :: FunctionSignature
  -> [(Int, [FunctionDomainInOutConstraints])]
  -> ExceptT Errors (State PropagatingContext) (Map Int [FunctionDomainInOutConstraints])
intersectPropagatedAndInitialConstraints signature [] = -- trace ("\nEMPTY: "++show signature) 
  return M.empty
intersectPropagatedAndInitialConstraints signature stictestConstraints = do
  context <- get
  let returnstictestConstraints = return $ M.fromList stictestConstraints
  --res <- 
  maybe
      returnstictestConstraints
      (\directives ->
        handleIntersectedDirectives signature directives $ callerAndCalleeIntersection directives stictestConstraints
      )
    $ M.lookup signature
    $ inputDirectives context
  -- return -- $ trace ("\nsignature: "++show signature++"\nres: "++show res) res

handleIntersectedDirectives
  :: FunctionSignature
  -> Map Int [FunctionDomainInOutConstraints]
  -> [(Int, [FunctionDomainInOutConstraints])]
  -> ExceptT Errors (State PropagatingContext) (Map Int [FunctionDomainInOutConstraints])
handleIntersectedDirectives signature directives directivesItersection =
  if existsNotNullConstraint directivesItersection
    then return $ M.union (M.fromList directivesItersection) directives
    else -- trace ("\ndirectivesIntersection: " ++ show directivesItersection) $ 
         throwFunctionIsUndefinedForInputOfItselfAndSubFunctions signature

existsNotNullConstraint :: [(Int, [FunctionDomainInOutConstraints])] -> Bool
existsNotNullConstraint = any (\(_, c) -> not $ null c)

propagateFunctionBodyOutput
  :: (FunctionSignature, [Statement])
  -> ExceptT Errors (State PropagatingContext) (Maybe [FunctionDomainInOutConstraints])
propagateFunctionBodyOutput (signature, []        ) = return Nothing
propagateFunctionBodyOutput (signature, statements) = lastStatementResult signature $ last statements

inputOutputIntersection
  :: (FunctionSignature, [Statement])
  -> [(Int, [FunctionDomainInOutConstraints])]
  -> Maybe [FunctionDomainInOutConstraints]
  -> ExceptT
       Errors
       (State PropagatingContext)
       (M.Map Int [FunctionDomainInOutConstraints], Maybe [FunctionDomainInOutConstraints])
inputOutputIntersection (signature, MkNoAssignment { expression = MkFunctionArgument _ MkRanged { rItem = arg } } : _) input@(_ : _) (Just output)
  = let
      inputConstraints = M.fromList input
      returnOutputOnly = return (M.empty, Just output)
    in
      maybe
          returnOutputOnly
          (\directive ->
            handleIntersectionNonEmptyInputOutput arg signature inputConstraints
              $ strictestConstraintsPerArg directive output
          )
        $ M.lookup arg inputConstraints

inputOutputIntersection (signature, MkNoAssignment { expression = MkFunctionArgument _ MkRanged { rItem = arg } } : _) inputConstraints@(_ : _) Nothing
  = let ic = M.fromList inputConstraints in return (ic, M.lookup arg ic)
inputOutputIntersection (signature, MkNoAssignment { expression = MkFunctionArgument _ MkRanged { rItem = arg } } : _) [] output@(Just o)
  = return (M.singleton arg o, output)
inputOutputIntersection (signature, _ : _) input _ = return (M.fromList input, Nothing)
inputOutputIntersection (signature, []   ) input _ = return (M.fromList input, Nothing)

handleIntersectionNonEmptyInputOutput
  :: Int
  -> FunctionSignature
  -> Map Int [FunctionDomainInOutConstraints]
  -> [FunctionDomainInOutConstraints]
  -> ExceptT
       Errors
       (State PropagatingContext)
       (Map Int [FunctionDomainInOutConstraints], Maybe [FunctionDomainInOutConstraints])
handleIntersectionNonEmptyInputOutput arg signature inputConstraints outputConstraints@(_ : _) =
  return (M.insert arg outputConstraints inputConstraints, Just outputConstraints)
handleIntersectionNonEmptyInputOutput arg signature inputConstraints [] =
  throwFunctionIsUndefForConstraintsOfArgumentAndReturn signature arg

lastStatementResult
  :: FunctionSignature
  -> Statement
  -> ExceptT Errors (State PropagatingContext) (Maybe [FunctionDomainInOutConstraints])
lastStatementResult sig MkNoAssignment { expression = MkFunctionArgument _ MkRanged { rItem = arg } } = do
  context <- get
  let ih = inputDirectives context
  return $ M.lookup arg =<< M.lookup sig ih
lastStatementResult sig MkNoAssignment { expression = MkConstantInteger _ MkRanged { rItem = c } } = do
  context <- get
  maybe
      (return Nothing)
      (\h ->
        if isConstantOutOfDomain c h then throwFunctionIsUndefinedWhenReturningConstantError sig c else return Nothing
      )
    $ M.lookup sig
    $ outputDirectives context
lastStatementResult sig MkNoAssignment { expression = MkFunctionCall { fcsName = MkRanged { rItem = name }, fcsArgs = args, tmpVarName = tvn } }
  = do
    context <- get
    let
      subSig =
        MkFunctionSignature { fsName = name, fsArgs = map (\_ -> MkValue "v") args, fsReturn = tmpVarToReturn tvn }
    let subFunctionMaybeDirective = lookup subSig $ outputDirectives context
    --trace ("\n\nsubFunctionMaybeDirective: "++show subFunctionMaybeDirective) 
    maybe
        (return Nothing)
        (\parentFunctionDirective ->
          if null $ snd $ strictestConstraintGroupsPerArg
            (0, parentFunctionDirective : maybeToList subFunctionMaybeDirective)
          then
            throwFunctionIsUndefeindWhenCallingSubFunction (fsName sig) name
          else
            return subFunctionMaybeDirective
        )
      $ M.lookup sig
      $ outputDirectives context

callerAndCalleeIntersection
  :: M.Map Int [FunctionDomainInOutConstraints]
  -> [(Int, [FunctionDomainInOutConstraints])]
  -> [(Int, [FunctionDomainInOutConstraints])]
callerAndCalleeIntersection callerConstraints calleeConstraints =
  let
    mergedConstraints = filter (\(f, s) -> not $ null s) $ map
      (\callee -> case M.lookup (fst callee) callerConstraints of
        Nothing -> if null $ snd callee then (fst callee, []) else (fst callee, [snd callee])
        Just c  -> (fst callee, [c, snd callee])
      )
      calleeConstraints
  in map strictestConstraintGroupsPerArg mergedConstraints

strictestConstraintGroupsPerArg :: (Int, [[FunctionDomainInOutConstraints]]) -> (Int, [FunctionDomainInOutConstraints])
strictestConstraintGroupsPerArg c =
  let res = foldl strictestConstraintsPerArg defaultConstraint $ snd c in (fst c, res)

strictestConstraintsPerArg
  :: [FunctionDomainInOutConstraints] -> [FunctionDomainInOutConstraints] -> [FunctionDomainInOutConstraints]
strictestConstraintsPerArg []         []          = []
strictestConstraintsPerArg []         (right : _) = []
strictestConstraintsPerArg (left : _) []          = []
strictestConstraintsPerArg left@(MkRangedFunctionDomainInOutConstraint { from = fl, to = tl } : ltail) right@(MkRangedFunctionDomainInOutConstraint { from = fr, to = tr } : rtail)
  | lessFromFrom fl fr && lessToFrom tl fr
  =  -- trace ("\nfrom fl < from fr && to tl < from fr: " ++ show left ++ ", " ++ show right) $  
    strictestConstraintsPerArg ltail right
  | lessFromFrom fl fr && lessToTo tr tl
  = -- trace ("\nfrom fl < from fr && to tr < to tl: " ++ show left ++ ", " ++ show right) $ 
    MkRangedFunctionDomainInOutConstraint { from = fr, to = tr } : strictestConstraintsPerArg left rtail
  | lessFromFrom fl fr && lessToTo tl tr
  = -- trace ("\nfrom fl < from fr && to tl < to tr: " ++ show left ++ ", " ++ show right) $ 
    MkRangedFunctionDomainInOutConstraint { from = fr, to = tl } : strictestConstraintsPerArg ltail right
  | lessFromFrom fl fr && lessToFrom tl fr && lessToTo tl tr
  = -- trace ("\nfrom fl < from fr && to tl < from fr && to tl < to tr: " ++ show left ++ ", " ++ show right) $ 
    MkRangedFunctionDomainInOutConstraint { from = fr, to = tl } : strictestConstraintsPerArg ltail right
  | lessFromFrom fr fl && lessFromTo fl tr && lessToTo tr tl
  = -- trace ("\nfrom fr < from fl && from fl < to tr && to tr < to tl: " ++ show left ++ ", " ++ show right) $ 
    MkRangedFunctionDomainInOutConstraint { from = fl, to = tr } : strictestConstraintsPerArg left rtail
  | lessFromFrom fr fl && lessToTo tl tr
  = -- trace ("\nfrom fr < from fl && to tl < to tr: " ++ show left ++ ", " ++ show right) $ 
    MkRangedFunctionDomainInOutConstraint { from = fl, to = tl } : strictestConstraintsPerArg ltail right
  | lessToFrom tr fl && lessToTo tr tl
  = -- trace ("\nto tr < from fl && to tr < to tl: "++ show left ++ ", " ++ show right) $ 
    strictestConstraintsPerArg left rtail
  | fl == fr && tl == tr
  = -- trace ("\nfl == fr && tl == tr: "++ show left ++ ", " ++ show right) $ 
    MkRangedFunctionDomainInOutConstraint { from = fl, to = tl } : strictestConstraintsPerArg rtail right
  | otherwise
  = panic ("Internal compiler error: " ++ show left ++ ", " ++ show right)

lessFromTo :: FunctionDomainConstraintFrom -> FunctionDomainConstraintTo -> Bool
lessFromTo MkNegInfinite        (MkIntegerTo _)  = True
lessFromTo (MkIntegerFrom _)    MkPosInfinite    = True
lessFromTo MkNegInfinite        MkPosInfinite    = True
lessFromTo (MkIntegerFrom from) (MkIntegerTo to) = from < to

lessToFrom :: FunctionDomainConstraintTo -> FunctionDomainConstraintFrom -> Bool
lessToFrom l r = not $ lessFromTo r l

lessFromFrom :: FunctionDomainConstraintFrom -> FunctionDomainConstraintFrom -> Bool
lessFromFrom MkNegInfinite     (MkIntegerFrom _) = True
lessFromFrom (MkIntegerFrom _) MkNegInfinite     = False
lessFromFrom MkNegInfinite     MkNegInfinite     = False
lessFromFrom (MkIntegerFrom l) (MkIntegerFrom r) = l < r

lessToTo :: FunctionDomainConstraintTo -> FunctionDomainConstraintTo -> Bool
lessToTo MkPosInfinite   (MkIntegerTo _) = False
lessToTo (MkIntegerTo _) MkPosInfinite   = True
lessToTo MkPosInfinite   MkPosInfinite   = False
lessToTo (MkIntegerTo l) (MkIntegerTo r) = l < r

propagateStatement
  :: Statement -> ExceptT Errors (State PropagatingContext) (M.Map Int [[FunctionDomainInOutConstraints]])
propagateStatement expr@(MkNoAssignment expression) = do
  result <- propagateExpression expression
  let
    res = case result of
      Left  _           -> M.empty
      Right constraints -> foldl (\acc (a, c) -> M.insert a (maybe [c] (c :) $ M.lookup a acc) acc) M.empty constraints
  return -- $ trace("\n\nexpression: "++show expr++"\nresult: "++show result++"\nres: "++show res) 
         res

propagateExpression
  :: Expression -> ExceptT Errors (State PropagatingContext) (Either Int [(Int, [FunctionDomainInOutConstraints])])
propagateExpression (MkConstantInteger  _ p                       ) = return $ Right []

propagateExpression (MkFunctionArgument _ MkRanged { rItem = arg }) = return $ Left arg

propagateExpression MkFunctionCall { fcsName = MkRanged { rItem = fsName }, fcsArgs = args, tmpVarName = tvn } = do
  let sig = MkFunctionSignature { fsName, fsArgs = map (\_ -> MkValue "v") args, fsReturn = tmpVarToReturn tvn }
  propagateBodyIfUnpropagated sig
  context <- get
  case M.lookup sig $ inputDirectives context of
    Just directives -> do
      result <- propagateArgs fsName $ fullFillArgs directives args
      let res = (Right $ L.concat result) :: (Either Int [(Int, [FunctionDomainInOutConstraints])])
      return  -- $ trace ("\nFOUND: "++show sig++", directives: "++show directives++"\nres: "++show res)  
             res
    _ ->  -- trace ("\nNOT FOUND: "++show sig++", directives: "++show (inputDirectives context)) 
      return $ Right []

fullFillArgs :: Map Int c -> [a] -> [(a, c)]
fullFillArgs directives =
  map (second fromJust) . filter (not . null . snd) . map (second (`M.lookup` directives) . swap) . indexed

propagateArgs
  :: Traversable t
  => Text
  -> t (Expression, [FunctionDomainInOutConstraints])
  -> ExceptT Errors (State PropagatingContext) (t [(Int, [FunctionDomainInOutConstraints])])
propagateArgs fsName = mapM
  (\(e, h) -> do
    case e of
      MkConstantInteger _ MkRanged { rItem = c } ->
        if isConstantOutOfDomain c h then throwFunctionIsUndefinedForConstaint fsName c else return ()
      _ -> return ()
    res <- propagateExpression e
    return $ either (\a -> [(a, h)]) id res
  )

isConstantOutOfDomain :: Integer -> [FunctionDomainInOutConstraints] -> Bool
isConstantOutOfDomain const = foldl (\acc c -> isConstantOutOfRange const c && acc) True

isConstantOutOfRange :: Integer -> FunctionDomainInOutConstraints -> Bool
isConstantOutOfRange c MkRangedFunctionDomainInOutConstraint { from = MkNegInfinite, to = MkPosInfinite }   = False
isConstantOutOfRange c MkRangedFunctionDomainInOutConstraint { from = MkNegInfinite, to = MkIntegerTo i }   = c > i
isConstantOutOfRange c MkRangedFunctionDomainInOutConstraint { from = MkIntegerFrom i, to = MkPosInfinite } = c < i
isConstantOutOfRange c MkRangedFunctionDomainInOutConstraint { from = MkIntegerFrom from, to = MkIntegerTo to } =
  c < from || c > to

propagateBodyIfUnpropagated :: FunctionSignature -> ExceptT Errors (State PropagatingContext) ()
propagateBodyIfUnpropagated sig = do
  context <- get
  if S.member sig (propagatedFunctions context) || S.member sig (callers context) || not
      (M.member sig $ functions context)
    then return ()
    else propagateFunctionBody (sig, justLookup sig $ functions context) >> return ()

throwFunctionIsUndefForConstraintsOfArgumentAndReturn
  :: FunctionSignature
  -> Int
  -> ExceptT
       Errors
       (State PropagatingContext)
       (Map Int [FunctionDomainInOutConstraints], Maybe [FunctionDomainInOutConstraints])
throwFunctionIsUndefForConstraintsOfArgumentAndReturn signature arg = throwError $ MkErrors
  [ MkError
      (T.concat
        [ "Function '"
        , fsName signature
        , "' is always undefined for the constraints intersection of argument '"
        , map unpackArg (fsArgs signature) !! arg
        , "' and return"
        ]
      )
  ]

throwFunctionIsUndefinedForInputOfItselfAndSubFunctions
  :: FunctionSignature -> ExceptT Errors (State PropagatingContext) (Map Int [FunctionDomainInOutConstraints])
throwFunctionIsUndefinedForInputOfItselfAndSubFunctions signature = throwError $ MkErrors
  [ MkError
      (T.concat
        [ "Function '"
        , fsName signature
        , "' is always undefined for the input directives of itself and the input directives of sub-functions"
        ]
      )
  ]

throwFunctionIsUndefinedForDomainIntersectionOftheInputDirectivesOfSubFunctions
  :: FunctionSignature -> ExceptT Errors (State PropagatingContext) (Map Int [FunctionDomainInOutConstraints])
throwFunctionIsUndefinedForDomainIntersectionOftheInputDirectivesOfSubFunctions signature = throwError $ MkErrors
  [ MkError
      (T.concat
        [ "Function '"
        , fsName signature
        , "' is always undefined for domain intersection of the input directives of sub-functions"
        ]
      )
  ]

throwFunctionIsUndefinedWhenReturningConstantError
  :: FunctionSignature -> Integer -> ExceptT Errors (State PropagatingContext) (Maybe [FunctionDomainInOutConstraints])
throwFunctionIsUndefinedWhenReturningConstantError sig c = throwError $ MkErrors
  [MkError (T.concat ["Function '", fsName sig, "' is always undefined when returning constant '", showT c, "'"])]

throwFunctionIsUndefeindWhenCallingSubFunction
  :: Text -> Text -> ExceptT Errors (State PropagatingContext) (Maybe [FunctionDomainInOutConstraints])
throwFunctionIsUndefeindWhenCallingSubFunction callerName toCallName = throwError $ MkErrors
  [MkError (T.concat ["Function '", callerName, "' is always undefined when calling sub function '", toCallName, "'"])]

throwFunctionIsUndefinedForConstaint :: Text -> Integer -> ExceptT Errors (State PropagatingContext) ()
throwFunctionIsUndefinedForConstaint fsName c = throwError $ MkErrors
  [MkError (T.concat ["Function '", fsName, "' is always undefined for constant argument '", showT c, "'"])]
