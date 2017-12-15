import LLVM.Prelude
import LLVM.Module
import LLVM.AST
import LLVM.AST.Global
import LLVM.Context

main = withContext $ \ctx -> do
  -- Obtain a Module for pietlib
  pietlibStr <- readFile "src/pietlib/pietlib.ll"
  pietlibMod <- withModuleFromLLVMAssembly ctx pietlibStr moduleAST

  putStrLn $ show pietlibMod
