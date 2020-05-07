{-# LANGUAGE TupleSections #-}
module Codegen
  ( genExpression
  , genFunctions
  , codegen
  , mkConstNumber
  , genStatement
  ) where

import Control.Monad.Except (ExceptT, MonadFix, foldM, liftIO)
import Control.Monad.Morph (lift)
import Data.ByteString.Char8 (pack, writeFile)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Map (Map, fromList, insert, insertWith, keys, lookup, singleton, toList)
import Data.Ord (Ord)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.List as L (length)
import GHC.Generics (Generic)
import LLVM.AST (FloatingPointType, Module, Name(Name), Operand(ConstantOperand), mkName, moduleSourceFileName)
import LLVM.AST.AddrSpace (AddrSpace)
import LLVM.AST.Constant (Constant(GlobalReference, Int))
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import LLVM.AST.Type (Type(FunctionType, IntegerType), i32, i64, i8, ptr, void)
import LLVM.Context (withContext)
import LLVM.IRBuilder.Constant (int32, int64)
import LLVM.IRBuilder.Instruction
  (alloca, bitcast, call, gep, globalStringPtr, load, ret, retVoid, sext, fcmp, icmp, br, phi)
import LLVM.IRBuilder.Module (ModuleBuilder, MonadModuleBuilder, ParameterName(ParameterName), buildModule, function)
import LLVM.IRBuilder.Monad (MonadIRBuilder, block, named)
import LLVM.Module (File(File), moduleLLVMAssembly, withModuleFromAST, writeObjectToFile)
import LLVM.Target (withHostTargetMachineDefault)
import CodeGenConfig (maxBitsPerCell, maxBitsCellType, intMaxBitsCellType)
import Control.Monad (when)
import Prelude
  ( Bool(False)
  , Either(Right)
  , Eq
  , IO
  , Integer
  , Int
  , Maybe(Nothing)
  , Show
  , String
  , (!!)
  , ($)
  , (++)
  , (.)
  , (/)
  , (+)
  , (>)
  , div
  , mod
  , concat
  , concatMap
  , const
  , foldl
  , head
  , map
  , mapM
  , putStrLn
  , return
  , reverse
  , show
  , snd
  )
import System.Directory (createDirectoryIfMissing)

import AspectsRegister (AnalyticsRegister, DirectivesRegister)
import EmbeddedFuncs (EmbeddedFuncs, printF, registerEmbeddedFuncs)
import Errors (Errors)
import ExternalFuncs
  (ExternFuncs, Func, fname, ftype, mallocF, mpInitF, mpRadixSizeF, mpReadRadixF, mpToRadixF, printfF, registerExtFuncs)
import ExternalTypes (ExtTypes, mpStruct, registerExtTypes)
import Location (Ranged(MkRanged, range), rItem)
import SemanticTree as Sem
  ( Arg(MkValue)
  , Expression(MkConstantInteger, MkFunctionArgument, MkFunctionCall, fcsArgs, fcsName)
  , Function(fBody, fSignature)
  , FunctionSignature(fsArgs, fsName, fsReturn)
  , NameArgs(MkNameArgs, naArgs, naName)
  , Return(NoReturn, Return)
  , SemanticModel(tree)
  , SemanticTree(_semanticTree)
  , Statement(expression)
  , extractFunctions
  )
import Data.Foldable (Foldable)
import Debug.Trace (trace)
import BooleanFormula (BooleanFormula(And))
import LLVM.AST.IntegerPredicate (IntegerPredicate(EQ))
import LLVM.IRBuilder (condBr)
import Panic (panic)

data FunctionCodegenSignature = MkFunctionCodegenSignature
  { cdgName   :: Name
  , cdgArgs   :: [(Type, ParameterName)]
  , cdgReturn :: Type
  }
  deriving (Eq, Generic, Show)

newtype EmbeddedFuncsOperand =
  MkEmbeddedFuncsOperand
    { printO :: Operand
    }
  deriving (Show)

data CodegenContext = MkCodegenContext
  { externTypes        :: ExtTypes
  , externFuncs        :: ExternFuncs
  , embeddedFuncs      :: EmbeddedFuncsOperand
  , cdgSignatures      :: Map NameArgs FunctionCodegenSignature
  , functions          :: Map NameArgs Operand
  , directivesRegister :: DirectivesRegister
  , analyticsRegister  :: AnalyticsRegister
  }

codegen :: String -> (EmbeddedFuncs, DirectivesRegister, AnalyticsRegister, SemanticModel) -> ExceptT Errors IO ()
codegen dir context = do
  liftIO $ putStrLn "\nGenerating target code..."
  lift $ toLLVM dir $ mainModule context

addEither :: IO () -> IO (Either Errors ())
addEither v = return $ Right ()

mainModule :: (EmbeddedFuncs, DirectivesRegister, AnalyticsRegister, SemanticModel) -> Module
mainModule context = (buildModule "application" $ moduleBuilder context) { moduleSourceFileName = "source file name" }

moduleBuilder :: (EmbeddedFuncs, DirectivesRegister, AnalyticsRegister, SemanticModel) -> ModuleBuilder ()
moduleBuilder (embeddedFuncs, directives, analytics, model) = do
  externTypes <- registerExtTypes
  externFuncs <- registerExtFuncs externTypes
  print       <- regPrint (printF embeddedFuncs) externTypes externFuncs
  let generatedEmbeddedFuncs = MkEmbeddedFuncsOperand { printO = print }
  genFunctions (externTypes, externFuncs, generatedEmbeddedFuncs, directives, analytics, model)

toLLVM :: String -> Module -> IO ()
toLLVM dir mod = withContext $ \ctx -> withHostTargetMachineDefault $ \tm -> do
  res <- withModuleFromAST ctx mod moduleLLVMAssembly
  createDirectoryIfMissing False $ dir ++ "/target"
  writeFile (dir ++ "/target/out.ll") res
  withModuleFromAST ctx mod (writeObjectToFile tm (File $ dir ++ "/target/out.o"))

genFunctions
  :: (ExtTypes, ExternFuncs, EmbeddedFuncsOperand, DirectivesRegister, AnalyticsRegister, SemanticModel)
  -> ModuleBuilder ()
genFunctions (externTypes, externFuncs, embeddedFuncs, directivesRegister, analyticsRegister, model) =
  let
    print     = printF registerEmbeddedFuncs
    mpStructP = ptr $ mpStruct externTypes
    funcs     = singleton (toNameArgs (mpStruct externTypes) print) $ printO embeddedFuncs
    functions = map (\f -> (f, toNameArgs (ptr $ mpStruct externTypes) $ fSignature f))
      $ concatMap snd (toList (_semanticTree $ tree model))
    cdgSig =
      fromList
        $ (toNameArgs (mpStruct externTypes) print, toCodegenFunctionSignature mpStructP print)
        : map (\(f, fs) -> (fs, toCodegenFunctionSignature mpStructP (fSignature f))) functions
    context = MkCodegenContext
      { externTypes
      , externFuncs        = externFuncs
      , embeddedFuncs
      , cdgSignatures      = cdgSig
      , functions          = funcs
      , directivesRegister
      , analyticsRegister
      }
  in mdo
    genMainFunc generatedFuncsContext
    generatedFuncsContext <- genFuncsSimple context functions
    return ()

genFuncsSimple :: (MonadModuleBuilder m, MonadFix m) => CodegenContext -> [(Function, NameArgs)] -> m CodegenContext
genFuncsSimple initialContext functions = mdo
  generatedList <- mapM (`genFunc` context) functions
  let context = foldl updateContext initialContext generatedList
  return context

updateContext :: CodegenContext -> (NameArgs, Operand) -> CodegenContext
updateContext context (fcs, o) =
  let
    et   = externTypes context
    ef   = embeddedFuncs context
    extF = externFuncs context
    funcs =
      insert MkNameArgs { naName = naName fcs, naArgs = map (const $ MkValue "v") $ naArgs fcs } o $ functions context
    cdg = cdgSignatures context
    hr  = directivesRegister context
    ar  = analyticsRegister context
  in MkCodegenContext
    { externTypes        = et
    , externFuncs        = extF
    , embeddedFuncs      = ef
    , functions          = funcs
    , cdgSignatures      = cdg
    , directivesRegister = hr
    , analyticsRegister  = ar
    }

toNameArgs :: Type -> FunctionSignature -> NameArgs
toNameArgs mpStructP fs = MkNameArgs { naName = fsName fs, naArgs = fsArgs fs }

toCodegenFunctionSignature :: Type -> FunctionSignature -> FunctionCodegenSignature
toCodegenFunctionSignature mpStructP fs =
  let
    args = fsArgs fs
    len  = L.length args
  in MkFunctionCodegenSignature
    { cdgName   = toName $ fsName fs
    , cdgArgs   = map (addTypeToArg mpStructP) args ++ map addTypeToDefArg [1 .. (definedFlagsAmount len)] -- TODO append integer args at the end. amount = input args amount / cell size + 1 if mod div is not null 
    , cdgReturn = case fsReturn fs of
      Return _ -> mpStructP
      NoReturn -> LLVM.AST.Type.void
    }

definedFlagsAmount :: Int -> Int
definedFlagsAmount len = len `div` maxBitsPerCell + (if len `mod` maxBitsPerCell > 0 then 1 else 0)

extractSignatures :: SemanticTree -> Map Text FunctionSignature
extractSignatures tree = fromList $ map (\f -> (fsName $ fSignature f, fSignature f)) $ extractFunctions tree

addTypeToArg :: Type -> Arg -> (Type, ParameterName)
addTypeToArg t i = case i of
  MkValue n -> (t, ParameterName $ toShort $ pack $ unpack n)

addTypeToDefArg :: Int -> (Type, ParameterName)
addTypeToDefArg i = (maxBitsCellType, ParameterName $ toShort $ pack $ "def_" ++ show i)

genMainFunc :: MonadModuleBuilder m => CodegenContext -> m Operand
genMainFunc context = mdo
  let mp     = ptr $ mpStruct $ externTypes context
  let printf = printfF $ externFuncs context
  function "main" [] i32 $ \[] -> do
    block `named` "main_entry"
    greetingsMessage <-
      globalStringPtr
          "The application has been generated by the Aspectation multi-language prototype v0.1\n"
          (Name "greetingsMessage")
        `named` "greetings message"
    greetingsMessageGep <- gep (ConstantOperand greetingsMessage) [int64 0] `named` "greetingsMessageGep"
    printRes            <- callF printf [(greetingsMessageGep, [])] `named` "printf_result"
    let main = fromJust $ lookup MkNameArgs { naName = "main", naArgs = [] } $ functions context
    call main []
    retVoid

mkConstNumber :: (MonadIRBuilder m, MonadModuleBuilder m) => Integer -> ExtTypes -> ExternFuncs -> m Operand
mkConstNumber intg types externs = do
  let mpStructP   = ptr $ mpStruct types
  let mpReadRadix = mpReadRadixF externs
  let malloc      = mallocF externs
  let mpInit      = mpInitF externs
  let namedArgs   = []
  let const24     = int64 24
  let const10     = int32 10
  constNumberStr <- globalStringPtr (show intg) (Name $ toShort (pack ("const_" ++ show intg))) `named` "constNumberStr"
  rawMemForConstNumber <- callF malloc [(const24, [])] `named` "rawMemForMpStruct"
  memForMpStruct <- bitcast rawMemForConstNumber mpStructP `named` "memForMpStruct"
  callF mpInit [(memForMpStruct, [])]
  numberStrBgn <- gep (ConstantOperand constNumberStr) [int64 0] `named` "numberStrBgn"
  mpReadResult <- callF mpReadRadix [(memForMpStruct, []), (numberStrBgn, []), (const10, [])]
  return memForMpStruct

genFunc :: MonadModuleBuilder m => (Function, NameArgs) -> CodegenContext -> m (NameArgs, Operand)
genFunc (func, sig) context =
  let
    cdgSig  = fromJust $ lookup sig $ cdgSignatures context
    retType = cdgReturn cdgSig
    name    = cdgName cdgSig
    args    = cdgArgs cdgSig
  in do
    func <- function name args retType $ \args -> do
      block `named` "entry"
      let statements = fBody func
      generatedStatements <- foldM (\acc s -> foldingStatements (acc, rItem s) args context) [] statements
      ret $ head generatedStatements
    return (sig, func)

genArgsType :: [Arg] -> ExtTypes -> [(Type, ParameterName)]
genArgsType args types = let mpStructP = ptr $ mpStruct types in map (addTypeToArg mpStructP) args

foldingStatements
  :: (MonadIRBuilder m, MonadModuleBuilder m) => ([Operand], Statement) -> [Operand] -> CodegenContext -> m [Operand]
foldingStatements (acc, s) args context = do
  statement <- genStatement s args context
  return $ statement : acc

genStatement :: (MonadIRBuilder m, MonadModuleBuilder m) => Statement -> [Operand] -> CodegenContext -> m Operand
genStatement statement args globalData = mdo
  let expr = expression statement
  generatedExpr <- genExpression expr args globalData
  return generatedExpr

genExpression :: (MonadIRBuilder m, MonadModuleBuilder m) => Expression -> [Operand] -> CodegenContext -> m Operand
genExpression (MkConstantInteger _ MkRanged { rItem = integer }) args context =
  mkConstNumber integer (externTypes context) (externFuncs context)
genExpression MkFunctionCall { fcsName = naName, fcsArgs = fcsArgs } args context =
  let
    mpPtr    = ptr $ mpStruct (externTypes context)
    naArgs   = map (const $ MkValue "v") fcsArgs
    sig      = MkNameArgs { naName = rItem naName, naArgs }
    function = fromJust $ lookup sig $ functions context
  in do
    argsToPass <- foldM (\acc s -> foldingExpressions (acc, s) args context) [] fcsArgs
    call function (reverse argsToPass ++ definedFlagArgs argsToPass) `named` "result"
    -- mkConstNumber 0 (externTypes context) (externFuncs context)
genExpression (MkFunctionArgument _ MkRanged { rItem = argPos }) args context = return $ args !! argPos

definedFlagArgs :: Foldable t => t a1 -> [(Operand, [ParameterAttribute])]
definedFlagArgs args = map ((, []) . const (intMaxBitsCellType 0)) [1 .. (definedFlagsAmount $ length args)]-- TODO mk defined values

foldingExpressions
  :: (MonadIRBuilder m, MonadModuleBuilder m)
  => ([(Operand, [ParameterAttribute])], Expression)
  -> [Operand]
  -> CodegenContext
  -> m [(Operand, [ParameterAttribute])]
foldingExpressions (acc, s) args globalData = do
  expression <- genExpression s args globalData
  return $ (expression, []) : acc

regPrint :: (MonadModuleBuilder m, MonadFix m) => FunctionSignature -> ExtTypes -> ExternFuncs -> m Operand
regPrint sig types externs = do
  let args            = fsArgs sig
  let name            = fsName sig
  let mpStructP       = ptr $ mpStruct types
  let definedFlagArgs = map addTypeToDefArg [1]
  let namedArgs = genArgsType args types ++ definedFlagArgs
  let mpRadixSize     = mpRadixSizeF externs
  let malloc          = mallocF externs
  let mpToRadix       = mpToRadixF externs
  let printf          = printfF externs
  function (toName name) namedArgs void $ \(gottenNumber : definedFlags) -> mdo
    when (length definedFlags > 1) $ panic "print function can't have more than 1 argument"
    block `named` "entry"
    let flag = head definedFlags
    cmp <- icmp EQ (int64 0) flag `named` "cmp"
    condBr cmp thenB elseB
    thenB  <- block `named` "then"
    thenOp <- do
      template    <- globalStringPtr "%s\n" (Name "printNumberTemplate") `named` "printNumberTemplate"
      templateGep <- gep (ConstantOperand template) [int64 0] `named` "templateGep"
      memForSize  <- alloca i32 Nothing 0 `named` "memForSize"
      callF mpRadixSize [(gottenNumber, []), (int32 10, []), (memForSize, [])]
      size            <- load memForSize 0 `named` "size"
      extSize         <- sext size i64 `named` "extSize"
      rawMemForNumStr <- callF malloc [(extSize, [])]
      mpToRadixResult <- callF mpToRadix [(gottenNumber, []), (rawMemForNumStr, []), (int32 10, [])]
      print           <- callF printf [(templateGep, []), (rawMemForNumStr, [])]
      return $ int64 0
    br mergeB
    elseB  <- block `named` "else"
    elseOp <- do
      undefinedMsgStr    <- globalStringPtr "undefined\n" (Name "undefinedMsgStr") `named` "undefinedMsgStr"
      undefinedMsgStrGep <- gep (ConstantOperand undefinedMsgStr) [int64 0] `named` "undefinedMsgStrGep"
      print              <- callF printf [(undefinedMsgStrGep, [])]
      return $ int64 0
    br mergeB
    mergeB <- block `named` "ifcont"
    phi [(thenOp, thenB), (elseOp, elseB)]
    retVoid

toName :: Text -> Name
toName = mkName . unpack

callF :: MonadIRBuilder m => Func -> [(Operand, [ParameterAttribute])] -> m Operand
callF func args = do
  let name         = fname func
  let unpackedName = unpack name
  let fType        = ftype func
  let resultName = toShort $ pack $ unpackedName ++ "_result"
  let pCall = call $ ConstantOperand $ GlobalReference fType $ mkName $ unpack name
  pCall args `named` resultName
