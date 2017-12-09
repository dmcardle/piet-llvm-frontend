module DebugPrinter where
--
-- Module for visualizing Piet programs on the command line.
--

import System.Console.ANSI

import qualified PietLang as P

putCodelStr color = do
  setSGR [SetColor (ansiGround color) (ansiVividness color) (ansiColor color),
          SetUnderlining (ansiUnderline color)]
  putStr "X"
  setSGR [Reset]

ansiGround (P.Color _ light) = case  light of
               P.Light -> Foreground
               P.Normal -> Foreground
               P.Dark -> Background

ansiColor (P.Color hue _) = case hue of
                            P.Red -> Red
                            P.Yellow -> Yellow
                            P.Green -> Green
                            P.Cyan -> Cyan
                            P.Blue -> Blue
                            P.Magenta -> Magenta
                            P.Black -> Black
                            P.White -> White

ansiVividness (P.Color _ light) =
  case light of
    P.Light -> Vivid
    P.Normal -> Dull
    P.Dark -> Dull

ansiUnderline (P.Color _ light) =
  case light of
    P.Light -> SingleUnderline
    _ -> NoUnderline
