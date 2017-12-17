module ImageLoader (translateImage,
                    PietProgram(..),
                    PietHue(..),
                    PietLightness(..),
                    PietColor(..)) where

import Codec.Picture
import Data.Maybe
import qualified Data.Vector.Storable as DVS

data PietProgram = PietProgram { codels :: [[PietColor]],
                                 width :: Int,
                                 height :: Int
                               } deriving (Show)

data PietHue = Red | Yellow | Green | Cyan | Blue | Magenta | Black | White
  deriving (Eq, Enum, Show)

data PietLightness = Light | Normal | Dark
  deriving (Eq, Enum, Show)

data PietColor = Color PietHue PietLightness
  deriving (Eq, Show)

translateImage :: FilePath -> IO(PietProgram)
translateImage p = do
  eImg <- readImage p :: IO(Either String DynamicImage)
  case eImg of
    Left msg -> error msg
    Right (ImageRGB8 img) -> do
      let Image {imageWidth=w, imageHeight=h} = img
          pixels = [[pixelAt img x y | x<-[0..w-1]] | y<-[0..h-1]]
          codels = map (map readCodel) $ pixels
      return $ PietProgram { codels = codels,
                             width = w,
                             height = h }
    Right _ -> error "Input must be an ImageRGB8"

readCodel :: PixelRGB8 -> PietColor
readCodel (PixelRGB8 r g b) = case (r,g,b) of
  (0xff,0xc0,0xc0) -> Color Red Light
  (0xff,0x00,0x00) -> Color Red Normal
  (0xc0,0x00,0x00) -> Color Red Dark
  (0xff,0xff,0xc0) -> Color Yellow Light
  (0xff,0xff,0x00) -> Color Yellow Normal
  (0xc0,0xc0,0x00) -> Color Yellow Dark
  (0xc0,0xff,0xc0) -> Color Green Light
  (0x00,0xff,0x00) -> Color Green Normal
  (0x00,0xc0,0x00) -> Color Green Dark
  (0xc0,0xff,0xff) -> Color Cyan Light
  (0x00,0xff,0xff) -> Color Cyan Normal
  (0x00,0xc0,0xc0) -> Color Cyan Dark
  (0xc0,0xc0,0xff) -> Color Blue Light
  (0x00,0x00,0xff) -> Color Blue Normal
  (0x00,0x00,0xc0) -> Color Blue Dark
  (0xff,0xc0,0xff) -> Color Magenta Light
  (0xff,0x00,0xff) -> Color Magenta Normal
  (0xc0,0x00,0xc0) -> Color Magenta Dark
  (0xff,0xff,0xff) -> Color White Normal
  (0x00,0x00,0x00) -> Color Black Normal
  _ -> Color Black Normal
