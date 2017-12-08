module Model where

data PietHue = Red | Yellow | Green | Cyan | Blue | Magenta | Black | White
data PietLightness = Light | Normal | Dark
data PietColor = Color PietHue PietLightness
