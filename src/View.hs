module View where
--
-- Module for visualizing Piet programs on the command line.
--

import System.Console.ANSI

import qualified Model as M

putCodelStr color = do
  setSGR [SetColor (ansiGround color) (ansiVividness color) (ansiColor color),
          SetUnderlining (ansiUnderline color)]
  putStr "X"
  setSGR [Reset]

ansiGround (M.Color _ light) = case  light of
               M.Light -> Foreground
               M.Normal -> Foreground
               M.Dark -> Background

ansiColor (M.Color hue _) = case hue of
                            M.Red -> Red
                            M.Yellow -> Yellow
                            M.Green -> Green
                            M.Cyan -> Cyan
                            M.Blue -> Blue
                            M.Magenta -> Magenta
                            M.Black -> Black
                            M.White -> White

ansiVividness (M.Color _ light) = case light of
               M.Light -> Vivid
               M.Normal -> Dull
               M.Dark -> Dull

ansiUnderline (M.Color _ light) = case light of
                                    M.Light -> SingleUnderline
                                    _ -> NoUnderline
