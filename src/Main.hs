import qualified View as V
import Model

allPietHue :: [PietHue]
allPietHue = [toEnum 0 ..]

allPietLightness :: [PietLightness]
allPietLightness = [toEnum 0 ..]

printAllHues light = do
  mapM_ V.putCodelStr [(Color hue light) | hue <- allPietHue]
  putStrLn $ " (" ++ show light ++ ")"

printLegend = mapM_ printAllHues allPietLightness

main = printLegend

