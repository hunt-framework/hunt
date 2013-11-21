module Holumbus.Common.Positions where

import qualified Data.IntSet                as IS

import           Holumbus.Common.BasicTypes

-- ----------------------------------------------------------------------------

-- | The positions of the word in the document.
type Positions       = IS.IntSet

-- | Empty positions.
empty                :: Positions
empty                = IS.empty

-- | Positions with one element.
singleton            :: Position -> Positions
singleton            = IS.singleton

-- | Whether the 'Position' is part of 'Positions'.
member               :: Position -> Positions -> Bool
member               = IS.member

-- | Converts 'Positions' to a list of 'Position's in ascending order.
toAscList            :: Positions -> [Position]
toAscList            = IS.toAscList

-- | Constructs Positions from a list of 'Position's.
fromList             :: [Position] -> Positions
fromList             = IS.fromList

-- | Number of 'Position's.
size                 :: Positions -> Int
size                 = IS.size

-- | The union of two 'Positions'.
union                :: Positions -> Positions -> Positions
union                = IS.union

-- | A fold over Positions
foldr                 :: (Position -> r -> r) -> r -> Positions -> r
foldr                 = IS.foldr

-- ----------------------------------------------------------------------------
