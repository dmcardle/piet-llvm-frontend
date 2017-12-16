{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import PietLang

-- Codegen takes a list of abstract color blocks and emits LLVM IR code.
-- Global state in the emitted code will include the DP and CC.

import Control.Monad.Except
import Data.Word
import Data.Char (ord)

import LLVM.Prelude
import LLVM.Module
import LLVM.Analysis
import LLVM.AST
import LLVM.AST.Global hiding (callingConvention, returnAttributes, functionAttributes)
import LLVM.AST.AddrSpace
import qualified LLVM.AST.InlineAssembly as ASM
import qualified LLVM.AST.CallingConvention as CC
import LLVM.Context
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Operand as Oper
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Linkage as L

import Control.Monad.Except
import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BC

  --import qualified Data.ByteString.Short as B.Short

int :: Type
int = IntegerType 32

concatAsName :: [String] -> Name
concatAsName xs = Name (BS.toShort $ B.concat $ map BC.pack xs)

colorBlockName :: String -> Name
colorBlockName blockId = concatAsName ["ColorBlock", blockId]

colorBlockParams = ( [ Parameter (T.ptr T.i32) (Name "stackptr") []
                     , Parameter T.i8 (Name "dp") []
                     , Parameter T.i8 (Name "cc") []
                     ]
                   , False )

globalPersonalityFunc = Const.GlobalReference (T.ptr (T.FunctionType T.VoidType [] False)) (Name "catchExc")

--colorTableTy = T.ptr $ ArrayType {nArrayElements = 8, elementType = T.i32}
colorTableTy = T.ptr T.i32


funcTableTy =
  (PointerType
   {pointerReferent = nextBlockFuncTy, pointerAddrSpace = AddrSpace 0})

nextBlockFuncTy =
  PointerType
  { pointerReferent =
      FunctionType
      { resultType = VoidType
      , argumentTypes =
          [ PointerType
            { pointerReferent = IntegerType {typeBits = 32}
            , pointerAddrSpace = AddrSpace 0
            }
          , IntegerType {typeBits = 8}
          , IntegerType {typeBits = 8}
          ]
      , isVarArg = False
      }
  , pointerAddrSpace = AddrSpace 0
  }

defNextBlockFunc :: Definition
defNextBlockFunc =
  GlobalDefinition
    functionDefaults
    { name = Name "nextBlock"
    , linkage = L.External
    , parameters =
        ( [ Parameter (T.ptr T.i32) (Name "stackPtr") []
          , Parameter T.i8 (Name "dp") []
          , Parameter T.i8 (Name "cc") []
          , Parameter funcTableTy (Name "funcTable") []
          , Parameter colorTableTy (Name "colorTable") []
          , Parameter T.i32 (Name "curBlockNum") []
          ]
        , False)
    , returnType = T.VoidType
    }

libNextBlockFuncTy =
  FunctionType
  { resultType = VoidType
  , argumentTypes = [T.ptr T.i32, T.i8, T.i8, funcTableTy, colorTableTy, T.i32]
  , isVarArg = False
  }

-- Define the exception handler
defCatchExc :: Definition
defCatchExc = GlobalDefinition functionDefaults
  { name = Name "catchExc"
  , linkage = L.Internal
  , parameters = ([], False)
  , returnType = T.VoidType
  , basicBlocks = [BasicBlock (Name "done") [] (Do $ Ret Nothing [])]
  }

-- Define the special black border. The block's number is agreed to be 0.
defBlackBorder :: Definition
defBlackBorder = GlobalDefinition functionDefaults
  { name = colorBlockName "0"
  , linkage = L.LinkOnce
  , personalityFunction = Just globalPersonalityFunc
  , parameters = colorBlockParams
  , returnType = T.VoidType
  , basicBlocks = [BasicBlock (Name "done") [] (Do $ Ret Nothing [])]
  }

defColorBlock :: AbsColorBlock -> Definition
defColorBlock c =
  GlobalDefinition
    functionDefaults
    { name = colorBlockName colorBlockId
    , linkage = L.LinkOnce
    , personalityFunction = Just globalPersonalityFunc
    , parameters = colorBlockParams
    , returnType = T.VoidType
    , basicBlocks = concat [setupBasicBlocks (nextBlockLookup c), exitBlocks]
    }
  where
    colorBlockId :: String
    colorBlockId = show $ bid $ rawBlock c
    --i8Var :: String -> Operand
    i8Var name = LocalReference T.i8 (Name name)
    stackptrVar = i8Var "stackptr"
    dpVar = i8Var "dp"
    ccVar = i8Var "cc"
    comboVar = i8Var "combo"
    setupBasicBlocks ::
         [(PietDirPointer, PietCodelChooser, ColorBlockId)] -> [BasicBlock]
    setupBasicBlocks lookupTbl =
      [ BasicBlock
          (concatAsName ["set_vars", colorBlockId])
          -- Make room for the CC value in combo
          ([ Name "dpShift" :=
             Shl True True dpVar (ConstantOperand (Const.Int 8 1)) []
           , Name "combo" := Add True True (i8Var "dpShift") ccVar []
           , Name "funcArray" :=
             Alloca
             { allocatedType =
                 ArrayType {nArrayElements = 8, elementType = nextBlockFuncTy}
             , numElements = Nothing
             , LLVM.AST.alignment = 16
             , metadata = []
             }
           , Name "colorArray" :=
             Alloca
             { allocatedType =
                 ArrayType {nArrayElements = 8, elementType = T.i32}
             , numElements = Nothing
             , LLVM.AST.alignment = 16
             , metadata = []
             }
           , Name "funcArrayPtr" :=
             GetElementPtr
             { inBounds = True
             , address =
                 LocalReference
                   (PointerType
                    { pointerReferent =
                        ArrayType
                        {nArrayElements = 8, elementType = nextBlockFuncTy}
                    , pointerAddrSpace = AddrSpace 0
                    })
                   (Name "funcArray")
             , indices =
                 [ ConstantOperand
                     (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
                 , ConstantOperand
                     (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
                 ]
             , metadata = []
             }
           , Name "colorArrayPtr" :=
             GetElementPtr
             { inBounds = True
             , address =
                 LocalReference
                   (PointerType
                    { pointerReferent =
                        ArrayType
                        {nArrayElements = 8, elementType = T.i32}
                    , pointerAddrSpace = AddrSpace 0
                    })
                   (Name "colorArray")
             , indices =
                 [ ConstantOperand
                     (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
                 , ConstantOperand
                     (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
                 ]
             , metadata = []
             }
           ] ++
           (concatMap storeInstructionsForCombo (zip [0 .. 7] lookupTbl)))
          (Do $ Br (Name "perform_action") [])
      , BasicBlock
          (Name "perform_action")
          [ Do $
            Call
            { tailCallKind = Nothing
            , callingConvention = CC.C
            , returnAttributes = []
            , function =
                (Right $
                 ConstantOperand $
                 Const.GlobalReference
                   (T.ptr libNextBlockFuncTy)
                   (Name "nextBlock"))
            , arguments =
                [(p, []) | p <- [stackptrVar, dpVar, ccVar]] ++
                  -- funcTable
                [ (LocalReference funcTableTy (Name "funcArrayPtr"), [])
                  -- colorTable
                , (LocalReference T.i32 (Name "colorArrayPtr"), [])
                  -- curBlockNum
                , ( ConstantOperand
                      (Const.Int 32 (fromIntegral (bid (rawBlock c))))
                  , [])
                ]
            , functionAttributes = []
            , metadata = []
            }
          ]
          (Do $ Ret Nothing [])
      ]
    storeInstructionsForCombo ::
         (Integer, (PietDirPointer, PietCodelChooser, ColorBlockId))
      -> [Named Instruction]
    storeInstructionsForCombo (n, (dp, cc, blockId)) =
      let funcLookupTableElemPtrName = concatAsName ["func_tbl_ptr", show n]
          colorLookupTableElemPtrName = concatAsName ["color_tbl_ptr", show n]
          combo = 2 * (fromEnum dp) + (fromEnum cc)
      in [ funcLookupTableElemPtrName :=
           GetElementPtr
           { inBounds = True
           , address =
               LocalReference
                 (PointerType
                  { pointerReferent =
                      ArrayType
                      {nArrayElements = 8, elementType = nextBlockFuncTy}
                  , pointerAddrSpace = AddrSpace 0
                  })
                 (Name "funcArray")
           , indices =
               [ ConstantOperand
                   (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
               , ConstantOperand
                   (Const.Int
                    { Const.integerBits = 32
                    , Const.integerValue = fromIntegral combo
                    })
               ]
           , metadata = []
           }
         , Do
             (Store
              { volatile = False
              , address =
                  LocalReference
                    (PointerType
                     { pointerReferent = nextBlockFuncTy
                     , pointerAddrSpace = AddrSpace 0
                     })
                    funcLookupTableElemPtrName
              , value =
                  ConstantOperand
                    (Const.GlobalReference
                       nextBlockFuncTy
                       (concatAsName ["ColorBlock", show blockId]))
              , maybeAtomicity = Nothing
              , LLVM.AST.alignment = 8
              , metadata = []
              }),
           colorLookupTableElemPtrName :=
           GetElementPtr
           { inBounds = True
           , address =
               LocalReference
                 (PointerType
                  { pointerReferent =
                      ArrayType
                      {nArrayElements = 8, elementType = T.i32}
                  , pointerAddrSpace = AddrSpace 0
                  })
                 (Name "colorArray")
           , indices =
               [ ConstantOperand
                   (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
               , ConstantOperand
                   (Const.Int
                    { Const.integerBits = 32
                    , Const.integerValue = fromIntegral combo
                    })
               ]
           , metadata = []
           }
         , Do
             (Store
              { volatile = False
              , address =
                  LocalReference
                    (PointerType
                     { pointerReferent = nextBlockFuncTy
                     , pointerAddrSpace = AddrSpace 0
                     })
                    colorLookupTableElemPtrName
              , value =
                  ConstantOperand
                    (Const.Int 32 0)
              , maybeAtomicity = Nothing
              , LLVM.AST.alignment = 8
              , metadata = []
              })
         ]
    exitBlocks :: [BasicBlock]
    exitBlocks =
      [ BasicBlock
          (concatAsName ["catch_exception"])
          []
          (Do $ Resume (ConstantOperand Const.TokenNone) [])
      , BasicBlock (concatAsName ["exit_normal"]) [] (Do $ Ret Nothing [])
      ]

createModule :: [AbsColorBlock] -> LLVM.AST.Module
createModule blocks@(firstBlock:_) = defaultModule
  { moduleName = "basic"
  , moduleDefinitions =  [defCatchExc, defNextBlockFunc, defBlackBorder] ++ map defColorBlock blocks
  }

toLLVM :: [AbsColorBlock]-> IO ()
toLLVM colorBlocks = do
  withContext $ \ctx -> do
    let colorBlocksModA = createModule colorBlocks
    colorBlocksAsm <- withModuleFromAST ctx colorBlocksModA (\m -> do
                                                                verify m
                                                                asm <- moduleLLVMAssembly m
                                                                return $ asm)

    writeFile "colorblocks.ll" (BC.unpack colorBlocksAsm)
