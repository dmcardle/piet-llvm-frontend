module PietLang where

import ImageLoader

import Data.Maybe
import Data.List (nub)
import Debug.Trace

type ColorBlockId = Int
type ColorBlockConn = Maybe ColorBlockId

-- The raw color block resembles tokens in a traditional programming language.
data RawColorBlock = RawColorBlock { bid :: ColorBlockId,
                                     color :: PietColor,
                                     size :: Int,
                                     members :: [(Int, Int)]
                                   }
  deriving (Eq, Show)

-- The abstract color block resembles an Abstract Syntax Tree.
data AbsColorBlock = AbsColorBlock { raw :: RawColorBlock,
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

nullRawColorBlock = RawColorBlock { bid=0,
                                    color=Color Black Normal,
                                    size=0,
                                    members=[]
                                  }

nullAbsColorBlock = AbsColorBlock { raw=nullRawColorBlock,
                      dp_r_cc_l=0,
                      dp_r_cc_r=0,
                      dp_d_cc_l=0,
                      dp_d_cc_r=0,
                      dp_l_cc_l=0,
                      dp_l_cc_r=0,
                      dp_u_cc_l=0,
                      dp_u_cc_r=0
                    }

nullProgram = PietProgram {codels = [], width = 0, height = 0}

testProgram = PietProgram {codels = [[Color Red Normal,
                                      Color Red Normal,
                                      Color Red Light],
                                     [Color Black Normal,
                                      Color Blue Light,
                                      Color Blue Light]],
                           width=3,
                           height=2}

-- Groups 4-connected components in the 2D matrix of codels into RawColorBlocks.
-- Also assigns names to the blocks for ease of debugging.
lexProg :: PietProgram -> [(Int,Int)] -> [RawColorBlock]
lexProg p@(PietProgram {codels=c, width=w, height=h}) visited = buildBlocks (enumCoords w h) [] 0
  where
    buildBlocks [] _ _ = []
    buildBlocks ((x,y):xys) seen cnt
      -- If the point is already used by another block, continue processing
      | elem (x,y) seen = buildBlocks xys seen cnt
      -- Build the block and add its members to seen
      | otherwise = thisBlock : buildBlocks xys ((members thisBlock)++seen) (cnt+1)
      where
        thisBlock = growBlock [(x,y)] [] nullRawColorBlock{bid=cnt, color=colorAt p (x,y)}

    growBlock :: [(Int,Int)] -> [(Int,Int)] -> RawColorBlock -> RawColorBlock
    growBlock [] _ block = block
    growBlock (pt@(x,y):qu) seen block@(RawColorBlock{size=s, members=ms, color=col})
      -- Pass over this point if we've already seen it
      | elem pt seen = growBlock qu seen block
      -- If the color is right, add it to the block!
      | col == (colorAt p pt) = growBlock (qu++(neighbors pt)) (pt:seen) block{size=s+1, members=(pt:ms)}
      -- Wrong color -- add it to seen and continue
      | otherwise = growBlock qu (pt:seen) block

    -- Defines a list of valid neighbors
    neighbors (x,y) = filter inBounds [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
    inBounds (x,y) = (0 <= x && x < w) && (0 <= y && y < h)

-- List all coordinates in the rectangle from (0,0) to (w-1, h-1)
enumCoords w h = [(x,y) | x <- [0..w-1], y <- [0..h-1]]

colorAt :: PietProgram -> (Int,Int) -> PietColor
colorAt PietProgram{codels=cs, width=w, height=h} (x,y)
  | x < 0 || x >= w = Color Black Normal
  | y < 0 || y >= h = Color Black Normal
  | otherwise = (cs !! y) !! x

-- Parse a list of RawColorBlocks into a syntax tree.
--
parse :: [RawColorBlock] -> [AbsColorBlock]
parse rs = map buildNeighbors rs
  where
    buildNeighbors r = nullAbsColorBlock
