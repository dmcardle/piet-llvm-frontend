{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import PietLang

-- Codegen takes a list of abstract color blocks and emits LLVM IR code.
-- Global state in the emitted code will include the DP and CC.

import LLVM.Prelude
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import qualified LLVM.AST.CallingConvention as CC
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Type as T

import Control.Monad.Except
import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Short as B.Short

int :: Type
int = IntegerType 32

defColorBlock :: AbsColorBlock -> Definition
defColorBlock c = GlobalDefinition functionDefaults
  { name = Name (BS.toShort $ B.concat ["colorBlock", BC.pack $ show $ bid $ rawBlock c])
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])

defMain :: Operand -> Definition
defMain fn = GlobalDefinition functionDefaults
  { name = Name "main"
  , parameters = ([], False)
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
           (Name "entry")
           [ Do $ Call Nothing CC.C [] (Right fn) [] [] [] ]
           (Do $ Ret (Just (ConstantOperand $ Const.Int 0 0)) [])

createModule :: [AbsColorBlock] -> AST.Module
createModule blocks@(firstBlock:_) = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defColorBlock b | b <- blocks] ++ [irMain]
  }
  where
    irBlockFuncs = [defColorBlock b | b <- blocks]

    firstBlockRef :: Operand
    firstBlockRef = ConstantOperand $ Const.GlobalReference
                    (T.FunctionType
                      T.IntegerType{typeBits=32} [
                        IntegerType {typeBits = 32},
                          IntegerType {typeBits = 32}]
                      False)
                    (Name "colorBlock1")

    irMain = defMain firstBlockRef

toLLVM :: [AbsColorBlock]-> IO ()
toLLVM colorBlocks = withContext $ \ctx -> do
  let mod = createModule colorBlocks
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  writeFile "out.ll" (BC.unpack llvm)
