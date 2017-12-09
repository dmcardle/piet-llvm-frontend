module PietLang where

import Codec.Picture
import Control.Lens.Traversal
import Data.Maybe
import qualified Data.Vector.Storable as DVS

data PietHue = Red | Yellow | Green | Cyan | Blue | Magenta | Black | White
  deriving (Enum, Show)

data PietLightness = Light | Normal | Dark
  deriving (Enum, Show)

data PietColor = Color PietHue PietLightness
  deriving (Show)

data PietProgram = PietProgram { codels :: [[PietColor]],
                                 width :: Int,
                                 height :: Int
                               } deriving (Show)

nullProgram = PietProgram {codels = [], width = 0, height = 0}

translateImage :: FilePath -> IO(PietProgram)
translateImage p = do
  eImg <- readImage p :: IO(Either String DynamicImage)
  case eImg of
    Left msg -> error msg
    Right (ImageRGB8 img) -> do
      let Image w h _ = img
      let codels = traverse (\x -> return readCodel x) $ imagePixels img
      return $ PietProgram { codels = [],
                             width = w,
                             height = h}
    Right _ -> error "Input must be an ImageRGB8"

readCodel :: PixelRGB8 -> [PietColor]
readCodel (PixelRGB8 r g b) = [Color Red Light]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks w xs = let (chunk,rest) = splitAt w xs
              in chunk : chunks w rest
