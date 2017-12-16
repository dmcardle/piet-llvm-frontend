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
defNextBlockFunc = GlobalDefinition functionDefaults
  { name = Name "nextBlock"
  , linkage = L.External
  , parameters = ([ Parameter (T.ptr T.i32) (Name "stackPtr") []
                   , Parameter T.i8 (Name "dp") []
                   , Parameter T.i8 (Name "cc") []
                   , Parameter funcTableTy (Name "funcTable") []
                   , Parameter T.i32 (Name "combo") []
                   , Parameter T.i32 (Name "oldHue") []
                   , Parameter T.i32 (Name "oldLightness") []
                   , Parameter T.i32 (Name "newHue") []
                   , Parameter T.i32 (Name "newLightness") []
                   , Parameter T.i32 (Name "curBlockNum") []
                   ], False)
  , returnType = T.VoidType
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
defColorBlock c = GlobalDefinition functionDefaults
  { name = colorBlockName colorBlockId
  , linkage = L.LinkOnce
  , personalityFunction = Just globalPersonalityFunc
  , parameters = colorBlockParams
  , returnType = T.VoidType
  , basicBlocks = concat [setupBasicBlocks
                         ,(createConditionals 1 (nextBlockLookup c))
                         ,exitBlocks]
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

    setupBasicBlocks :: [BasicBlock]
    setupBasicBlocks = [
      BasicBlock (concatAsName ["set_vars", colorBlockId])
        ([
          -- Make room for the CC value in combo
          Name "dpShift" := Shl True True dpVar (ConstantOperand (Const.Int 8 1)) [],
          Name "combo" := Add True True (i8Var "dpShift") ccVar [],
          Name "funcArray" :=
               Alloca
               { allocatedType =
                   ArrayType
                   { nArrayElements = 8
                   , elementType = nextBlockFuncTy
                   }
               , numElements = Nothing
               , LLVM.AST.alignment = 16
               , metadata = []
               },
          Name "funcArrayPtr" :=
               GetElementPtr
               { inBounds = True
               , address =
                   LocalReference
                     (PointerType
                      { pointerReferent =
                          ArrayType
                          { nArrayElements = 8
                          , elementType = nextBlockFuncTy
                          }
                      , pointerAddrSpace = AddrSpace 0
                      })
                     (Name "funcArray")
               , indices =
                   [ ConstantOperand (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
                   , ConstantOperand (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
                   ]
               , metadata = []
               }
          ] ++ (concatMap storeInstructionsForCombo [0..7]))
        (Do $ Br (Name "perform_action") []),

      BasicBlock (Name "perform_action")
        [Do $ Call {
            tailCallKind=Nothing
            , callingConvention=CC.C
            ,returnAttributes=[]
            ,function=(Right $
                        ConstantOperand $
                        Const.GlobalReference
                        (T.ptr
                         (T.FunctionType T.VoidType
                          ([(T.ptr T.i32), T.i8, T.i8, funcTableTy] ++ (replicate 6 T.i32)) False)) (Name "nextBlock"))
            ,arguments=[(p,[]) | p<-[stackptrVar, dpVar, ccVar]] ++ [(LocalReference funcTableTy (Name "funcArrayPtr"), [])] ++ replicate 6 ((ConstantOperand $ Const.Int 32 0),[])
            ,functionAttributes=[]
            ,metadata=[]
            }]
        (Do $ Br (Name "decide1") [])
      ]

    storeInstructionsForCombo n =
      let lookupTableElemPtrName = concatAsName ["tmp_tbl_ptr", show n]
      in [lookupTableElemPtrName :=
           GetElementPtr
           { inBounds = True
           , address =
               LocalReference
                 (PointerType
                  { pointerReferent =
                      ArrayType {nArrayElements = 8, elementType = nextBlockFuncTy}
                  , pointerAddrSpace = AddrSpace 0
                  })
                 (Name "funcArray")
           , indices =
               [ ConstantOperand
                   (Const.Int {Const.integerBits = 32, Const.integerValue = 0})
               , ConstantOperand
                   (Const.Int {Const.integerBits = 32, Const.integerValue = n})
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
                    lookupTableElemPtrName
              , value =
                  ConstantOperand
                    (Const.GlobalReference nextBlockFuncTy (Name "ColorBlock1"))
              , maybeAtomicity = Nothing
              , LLVM.AST.alignment = 8
              , metadata = []
              })]

    -- Create a list of LLVM basic blocks that implement this color block's
    -- next-block lookup table.
    createConditionals :: Int -> [(PietDirPointer, PietCodelChooser, ColorBlockId)] -> [BasicBlock]
    createConditionals _ [] = []
    createConditionals n ((dp,cc,blk):xs) = newBasicBlocks ++ createConditionals (n+1) xs
      where
        expectedComboVal = 2*(fromEnum dp) + (fromEnum cc)
        expectedComboVar = concatAsName $ ["combo_is_", show expectedComboVal]

        nextCondName
          | xs == [] = Name "exit_normal"
          | otherwise = concatAsName ["decide", show (n+1)]

        newBasicBlocks = [
          BasicBlock (concatAsName ["decide", show n])
            [
              expectedComboVar := ICmp IntPred.EQ comboVar (ConstantOperand (Const.Int 8 0)) []
            ]
            (Do $ CondBr (LocalReference T.i8 expectedComboVar) (concatAsName ["handle", show expectedComboVal]) nextCondName [])
          ,
          BasicBlock (concatAsName ["handle", show expectedComboVal])
            [Do $ Call {
                tailCallKind=Just MustTail
                , callingConvention=CC.C
                ,returnAttributes=[]
                ,function=(Right $ ConstantOperand $ Const.GlobalReference (T.ptr (T.FunctionType T.VoidType [(T.ptr T.i32), T.i8, T.i8] False)) (colorBlockName $ show blk))
                ,arguments=[(p,[]) | p<-[stackptrVar, dpVar, ccVar]]
                ,functionAttributes=[]
                ,metadata=[]
                }]
            (Do $ Ret Nothing [])
          ]

    exitBlocks :: [BasicBlock]
    exitBlocks = [
          BasicBlock (concatAsName ["catch_exception"])
            []
            (Do $ Resume (ConstantOperand Const.TokenNone) []),
          BasicBlock (concatAsName ["exit_normal"])
            []
            (Do $ Ret Nothing [])
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
