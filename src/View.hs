module View where
--
-- Module for visualizing Piet programs on the command line.
--

import System.Console.ANSI (SGR(Reset,SetColor), ConsoleLayer(Background,Foreground), ColorIntensity(Dull,Vivid), Color(Red, Yellow, Green, Blue, Cyan, Magenta, Black, White), setSGR)

import qualified Model as M

printCodel color = do
  setSGR [SetColor (ansiGround color) (ansiVividness color) (ansiColor color)]
  putStrLn "X"
  setSGR [Reset]

ansiColor (M.Color hue _) = case hue of
                            M.Red -> Red
                            M.Yellow -> Yellow
                            M.Green -> Green
                            M.Cyan -> Cyan
                            M.Blue -> Blue
                            M.Magenta -> Magenta
                            M.Black -> Black
                            M.White -> White
ansiGround (M.Color _ light) = case  light of
               M.Light -> Foreground
               M.Normal -> Foreground
               M.Dark -> Background

ansiVividness (M.Color _ light) = case light of
               M.Light -> Vivid 
               M.Normal -> Dull
               M.Dark -> Dull
