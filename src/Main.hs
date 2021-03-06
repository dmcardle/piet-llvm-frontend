import qualified DebugPrinter as D
import PietLang
import ImageLoader
import Codegen

import System.Environment

allPietHue :: [PietHue]
allPietHue = [toEnum 0 ..]

allPietLightness :: [PietLightness]
allPietLightness = [toEnum 0 ..]

printAllHues light = do
  mapM_ D.putCodelStr [(Color hue light) | hue <- allPietHue]
  putStrLn $ " (" ++ show light ++ ")"

printLegend = mapM_ printAllHues allPietLightness

main = do
  (filePath:args) <- getArgs

  putStrLn "Legend:"
  printLegend
  putStrLn ""

  pietProg <- translateImage $ filePath
  D.printProg pietProg

  let colorBlocks = parse $ lexProg pietProg []

  --putStrLn "Color blocks:"
  --putStrLn $ show colorBlocks
  writeFile "autogen_colorblocks.hs" (show colorBlocks)

  toLLVM colorBlocks
