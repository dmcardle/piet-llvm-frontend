module PietLang where

import ImageLoader

import Data.Maybe

type ColorBlockId = String
type ColorBlockConn = Maybe ColorBlockId

data PietLexeme = ColorBlock { id :: ColorBlockId,
                               color :: PietColor,
                               size :: Int,
                               dp_r_cc_l :: ColorBlockId,
                               dp_r_cc_r :: ColorBlockId,
                               dp_d_cc_l :: ColorBlockId,
                               dp_d_cc_r :: ColorBlockId,
                               dp_l_cc_l :: ColorBlockId,
                               dp_l_cc_r :: ColorBlockId,
                               dp_u_cc_l :: ColorBlockId,
                               dp_u_cc_r :: ColorBlockId
                             }

data PietSyntaxTree = PietSyntaxTree

nullProgram = PietProgram {codels = [], width = 0, height = 0}

lex :: PietProgram -> [PietLexeme]
lex _ = []

parse :: [PietLexeme] -> PietSyntaxTree
parse _ = PietSyntaxTree
