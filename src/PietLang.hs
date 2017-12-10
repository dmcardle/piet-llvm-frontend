module PietLang where

import ImageLoader

import Data.Maybe
import Data.List (nub)

type ColorBlockId = Int
type ColorBlockConn = Maybe ColorBlockId

data ColorBlock = ColorBlock { bid :: ColorBlockId,
                               color :: PietColor,
                               size :: Int,
                               members :: [(Int, Int)],
                               dp_r_cc_l :: ColorBlockId,
                               dp_r_cc_r :: ColorBlockId,
                               dp_d_cc_l :: ColorBlockId,
                               dp_d_cc_r :: ColorBlockId,
                               dp_l_cc_l :: ColorBlockId,
                               dp_l_cc_r :: ColorBlockId,
                               dp_u_cc_l :: ColorBlockId,
                               dp_u_cc_r :: ColorBlockId
                             }
  deriving (Eq, Show)

nullColorBlock = ColorBlock {bid=0, color=Color Black Normal, size=0,
                               members=[],
                               dp_r_cc_l=0,
                               dp_r_cc_r=0,
                               dp_d_cc_l=0,
                               dp_d_cc_r=0,
                               dp_l_cc_l=0,
                               dp_l_cc_r=0,
                               dp_u_cc_l=0,
                               dp_u_cc_r=0
                            }

data PietSyntaxTree = PietSyntaxTree

nullProgram = PietProgram {codels = [], width = 0, height = 0}

testProgram = PietProgram {codels = [[Color Red Normal,
                                      Color Red Normal,
                                      Color Red Light],
                                     [Color Black Normal,
                                      Color Blue Light,
                                      Color Blue Light]],
                           width=3,
                           height=2}

-- Groups 4-connected components in the 2D matrix of codels into ColorBlocks.
-- Also assigns names to the blocks for ease of debugging.
lexProg :: PietProgram -> [(Int,Int)] -> [ColorBlock]
lexProg p@(PietProgram {codels=c, width=w, height=h}) visited = buildBlocks (enumCoords w h) 0
  where
    buildBlocks ((x,y):xys) cnt
      | elem (x,y) visited = []
      | otherwise = [growBlock [(x,y)] []
                      nullColorBlock{bid=cnt,
                                     color=colorAt p (x,y)}] -- just try building one

    growBlock [] _ block = block
    growBlock (pt@(x,y):qu) seen block@(ColorBlock{size=s, members=ms, color=c})
      -- Pass over this point if we've already seen it
      | elem pt seen = growBlock qu seen block
      -- If the color is right, add it to the block!
      | c == (colorAt p pt) = growBlock (qu++(neighbors pt)) (pt:seen) block{size=s+1, members=(pt:ms)}
      -- Wrong color -- add it to seen and continue
      | otherwise = growBlock qu (pt:seen) block
      where
        neighbors (x,y) = [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]


mergeBlocks :: [ColorBlock] -> ColorBlock
mergeBlocks blocks@(c:cs) = c{size=length uniqMembers, members=uniqMembers}
  where
    uniqMembers = nub $ concat $ map (\ColorBlock {members=ms} -> ms) blocks


enumCoords w h = [(x,y) | x <- [0..w-1], y <- [0..h-1]]

colorAt :: PietProgram -> (Int,Int) -> PietColor
colorAt PietProgram{codels=cs, width=w, height=h} (x,y)
  | x < 0 || x >= w = Color Black Normal
  | y < 0 || y >= h = Color Black Normal
  | otherwise = (cs !! y) !! x

-- Parse a list of ColorBlocks into a syntax tree.
--
parse :: [ColorBlock] -> PietSyntaxTree
parse _ = PietSyntaxTree
