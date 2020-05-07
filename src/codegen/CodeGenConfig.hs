module CodeGenConfig
  ( maxBitsPerCell
  , maxBitsCellType
  , intMaxBitsCellType
  ) where

import LLVM.AST.Type (Type, i64)
import LLVM.IRBuilder.Constant (int64)
import LLVM.AST.Operand (Operand)
maxBitsPerCell :: Int
maxBitsPerCell = 64

maxBitsCellType :: Type
maxBitsCellType = i64

intMaxBitsCellType :: Integer -> Operand
intMaxBitsCellType = int64
