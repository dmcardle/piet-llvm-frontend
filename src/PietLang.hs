module PietLang where

import ImageLoader

import Data.Maybe
import Data.List (nub, sort)
import Debug.Trace

type ColorBlockId = Int
type ColorBlockConn = Maybe ColorBlockId

data PietDirPointer = DP_Left | DP_Right | DP_Up | DP_Down deriving (Eq, Enum, Show)
data PietCodelChooser = CC_Left | CC_Right deriving (Eq, Enum, Show)

-- The raw color block resembles tokens in a traditional programming language.
data RawColorBlock = RawColorBlock { bid :: ColorBlockId,
                                     color :: PietColor,
                                     size :: Int,
                                     members :: [(Int, Int)]
                                   }
  deriving (Eq, Show)

-- The abstract color block resembles an Abstract Syntax Tree.
data AbsColorBlock = AbsColorBlock { rawBlock :: RawColorBlock,
                                     -- no point in making this a function since it must be exhaustive
                                     nextBlockLookup :: [(PietDirPointer, PietCodelChooser, ColorBlockId)]
                                   }
  deriving (Eq, Show)

enumAll :: (Enum a) => [a]
enumAll = [toEnum 0 ..]

-- Parse a list of RawColorBlocks into a syntax tree.
--
parse :: [RawColorBlock] -> [AbsColorBlock]
parse rs = map buildAbsBlock rs
  where
    buildAbsBlock r = AbsColorBlock {rawBlock=r, nextBlockLookup=buildLookup r}
    buildLookup r = [(dp, cc, findBlockFor dp cc $ chooseCodel cc $ getEdge dp)
                    | dp <- enumAll, cc <- enumAll]

      where
        pts = members r
        uniqX = nub $ sort $ map fst pts
        uniqY = nub $ sort $ map snd pts
        filterX v = [(x,y) | (x,y) <- pts, x==v]
        filterY v = [(x,y) | (x,y) <- pts, y==v]

        getEdge :: PietDirPointer -> [(Int, Int)]
        getEdge DP_Right = [ maximum (filterX x) | x <- uniqX ]
        getEdge DP_Down  = [ maximum (filterY y) | y <- uniqY ]
        getEdge DP_Left  = [ minimum (filterY y) | y <- uniqY ]
        getEdge DP_Up    = [ minimum (filterX x) | x <- uniqX ]

        -- This function implements part of the behavior described in the
        -- "Program Execution" section of the Piet language specification.
        findBlockFor :: PietDirPointer -> PietCodelChooser -> (Int, Int) -> ColorBlockId
        findBlockFor dp cc (x,y) = case dp of
                                     DP_Right -> blockAt (x+1, y)
                                     DP_Down  -> blockAt (x, y+1)
                                     DP_Left  -> blockAt (x-1, y)
                                     DP_Up    -> blockAt (x, y-1)

        -- Find the id of the ColorBlock that contains the given point.
        blockAt :: (Int, Int) -> ColorBlockId
        blockAt pt = case filter ((elem pt).members) rs of
          (b:_) -> bid b
          [] -> bid blackBorderColorBlock

        chooseCodel :: PietCodelChooser -> ([a] -> a)
        chooseCodel CC_Left = head
        chooseCodel CC_Right = last

-- Groups 4-connected components in the 2D matrix of codels into RawColorBlocks.
-- Also assigns names to the blocks for ease of debugging.
lexProg :: PietProgram -> [(Int,Int)] -> [RawColorBlock]
lexProg p@(PietProgram {codels=c, width=w, height=h}) visited = buildBlocks (enumCoords w h) [] 1
  where
    -- Count starts at 1 because of the surrounding black border.
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

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

blackBorderColorBlock = RawColorBlock { bid=0,
                                        color=Color Black Normal,
                                        size=0,
                                        members = []}

nullRawColorBlock = RawColorBlock { bid=0,
                                    color=Color Black Normal,
                                    size=0,
                                    members=[]
                                  }

nullAbsColorBlock = AbsColorBlock { rawBlock = nullRawColorBlock,
                                    nextBlockLookup = []
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
