module Holumbus.Common.Positions where

import qualified Data.IntSet as IS

import           Holumbus.Common.BasicTypes

-- | The positions of the word in the document.
type Positions                  = IS.IntSet

-- | Empty positions.
emptyPos                :: Positions
emptyPos                = IS.empty

-- | Positions with one element.
singletonPos            :: Position -> Positions
singletonPos            = IS.singleton

-- | Whether the 'Position' is part of 'Positions'.
memberPos               :: Position -> Positions -> Bool
memberPos               = IS.member

-- | Converts 'Positions' to a list of 'Position's in ascending order.
toAscListPos            :: Positions -> [Position]
toAscListPos            = IS.toAscList

-- | Constructs Positions from a list of 'Position's.
fromListPos             :: [Position] -> Positions
fromListPos             = IS.fromList

-- | Number of 'Position's.
sizePos                 :: Positions -> Int
sizePos                 = IS.size

-- | The union of two 'Positions'.
unionPos                :: Positions -> Positions -> Positions
unionPos                = IS.union

-- | A fold over Positions
foldPos                 :: (Position -> r -> r) -> r -> Positions -> r
foldPos                 = IS.fold

-- ------------------------------------------------------------
