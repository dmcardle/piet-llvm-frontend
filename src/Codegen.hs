{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import PietLang

-- Codegen takes a list of abstract color blocks and emits LLVM IR code.
-- Global state in the emitted code will include the DP and CC.

import LLVM.Prelude
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.InlineAssembly as ASM
import qualified LLVM.AST.CallingConvention as CC
import LLVM.Context
import LLVM.Module as M
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Operand as Oper
import qualified LLVM.AST.IntegerPredicate as IntPred

import Control.Monad.Except
import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Short as B.Short

int :: Type
int = IntegerType 32

defColorBlock :: AbsColorBlock -> Definition
defColorBlock c = GlobalDefinition functionDefaults
  { name = Name (BS.toShort $ B.concat ["colorBlock", BC.pack colorBlockId])
  , parameters =
      ( [ Parameter (T.ptr T.i32) (Name "stackptr") []
        , Parameter T.i8 (Name "dp") []
        , Parameter T.i8 (Name "cc") []
        ]
      , False )
  , returnType = int
  , basicBlocks = createConditionals $ nextBlockLookup c
  }
  where
    colorBlockId = show $ bid $ rawBlock c

    i8Var name = LocalReference T.i8 (Name name)
    dpVar = i8Var "dp"
    ccVar = i8Var "cc"
    comboVar = i8Var "combo"

    createConditionals :: [(PietDirPointer, PietCodelChooser, ColorBlockId)] -> [BasicBlock]
    createConditionals [] = []
    createConditionals ((dp,cc,blk):xs) = newBasicBlocks
      where
        newBasicBlocks = [
          BasicBlock (Name "basic_block")
            [
              {-
              Name "dp_check0" := ICmp IntPred.EQ dpVar (ConstantOperand (Const.Int 8 0)) [],
              Name "dp_check1" := ICmp IntPred.EQ dpVar (ConstantOperand (Const.Int 8 1)) [],
              Name "dp_check2" := ICmp IntPred.EQ dpVar (ConstantOperand (Const.Int 8 2)) [],
              Name "dp_check3" := ICmp IntPred.EQ dpVar (ConstantOperand (Const.Int 8 3)) [],
              Name "cc_check0" := ICmp IntPred.EQ ccVar (ConstantOperand (Const.Int 8 0)) [],
              Name "cc_check1" := ICmp IntPred.EQ ccVar (ConstantOperand (Const.Int 8 1)) [],
-}
              Name "dpShift" := Shl True True dpVar (ConstantOperand (Const.Int 8 3)) [],
              Name "combo" := Add True True (i8Var "dpShift") ccVar [],
              Name "combo_0" := ICmp IntPred.EQ comboVar (ConstantOperand (Const.Int 8 0)) []
            ]
            (Do $ CondBr comboVar (Name "trueDest") (Name "falseDest") [])
          ]



createModule :: [AbsColorBlock] -> LLVM.AST.Module
createModule blocks@(firstBlock:_) = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = map defColorBlock blocks
  }

toLLVM :: [AbsColorBlock]-> IO ()
toLLVM colorBlocks = withContext $ \ctx -> do
  let mod = createModule colorBlocks
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  writeFile "colorblocks.ll" (BC.unpack llvm)
