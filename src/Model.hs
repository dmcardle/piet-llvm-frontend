module Model where

data PietHue = Red | Yellow | Green | Cyan | Blue | Magenta | Black | White
  deriving (Enum, Show)

data PietLightness = Light | Normal | Dark
  deriving (Enum, Show)

data PietColor = Color PietHue PietLightness
