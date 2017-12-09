module DebugPrinter where
--
-- Module for visualizing Piet programs on the command line.
--

import System.Console.ANSI

import qualified PietLang as P

--printProg :: P.PietProgram -> IO ()
printProg P.PietProgram {P.codels=cs} = mapM_ printCodelRow cs
  where
    printCodelRow cs = do
      mapM_ putCodelStr cs
      putStrLn ""

putCodelStr color = do
  setSGR [SetColor (ansiGround color) (ansiVividness color) (ansiColor color),
          SetUnderlining (ansiUnderline color)]
  putStr $ strToPrint color
  setSGR [Reset]

strToPrint (P.Color _ light) = case light of
                                 P.Light -> " "
                                 P.Normal -> "N"
                                 P.Dark -> "_"

ansiGround (P.Color _ light) = case  light of
               P.Light -> Background
               P.Normal -> Foreground
               P.Dark -> Foreground

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
