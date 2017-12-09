module DebugPrinter where
--
-- Module for visualizing Piet programs on the command line.
--

import qualified System.Console.ANSI as A

import PietLang
import ImageLoader

--printProg :: PietProgram -> IO ()
printProg PietProgram {codels=cs} = mapM_ printCodelRow cs
  where
    printCodelRow cs = do
      mapM_ putCodelStr cs
      putStrLn ""

putCodelStr color = do
  A.setSGR [A.SetColor (ansiGround color)
             (ansiVividness color)
             (ansiColor color),
            A.SetUnderlining (ansiUnderline color)]
  putStr $ strToPrint color
  A.setSGR [A.Reset]

strToPrint (Color _ light) = case light of
                                 Light -> " "
                                 Normal -> "N"
                                 Dark -> "_"

ansiGround (Color _ light) = case  light of
               Light -> A.Background
               Normal -> A.Foreground
               Dark -> A.Foreground

ansiColor (Color hue _) = case hue of
                            Red -> A.Red
                            Yellow -> A.Yellow
                            Green -> A.Green
                            Cyan -> A.Cyan
                            Blue -> A.Blue
                            Magenta -> A.Magenta
                            Black -> A.Black
                            White -> A.White

ansiVividness (Color _ light) =
  case light of
    Light -> A.Vivid
    Normal -> A.Dull
    Dark -> A.Dull

ansiUnderline (Color _ light) =
  case light of
    Light -> A.SingleUnderline
    _ -> A.NoUnderline
