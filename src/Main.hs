import qualified View as V
import Model

allHues = [Red, Yellow, Green, Cyan, Blue, Magenta, Black, White]
allLights = [Light, Normal, Dark]

main = do
	mapM_ V.printCodel [ (Color hue light) | hue <- allHues, light <- allLights ]

