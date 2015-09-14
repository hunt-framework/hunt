{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Positions within document.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Positions where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary            as B
import qualified Data.IntSet.Packed     as IS
import           Data.IntSet.Cache      as IS
import           Data.Maybe             (fromMaybe)
import           Data.Typeable

import           Hunt.Common.BasicTypes

-- ------------------------------------------------------------

-- | The positions of the word in the document.

newtype Positions
    = PS {unPS :: IS.IntSet}
      deriving (Eq, Ord, Read, Show, Typeable, NFData, Monoid)

-- ------------------------------------------------------------

instance B.Binary Positions where
    put = B.put . toAscList
    get = fromList <$> B.get

instance ToJSON Positions where
    toJSON = toJSON . unPS

-- ------------------------------------------------------------

-- | Empty positions.
empty                :: Positions
empty                = PS IS.empty

-- | Positions with one element.
singleton            :: Position -> Positions
singleton            = PS . IS.cacheAt
--singleton            = PS . IS.singleton

-- | Test whether it is the empty positions.
null                 :: Positions -> Bool
null                 = IS.null . unPS

-- | Whether the 'Position' is part of 'Positions'.
member               :: Position -> Positions -> Bool
member p             = IS.member p . unPS

-- | Converts 'Positions' to a list of 'Position's in ascending order.
toAscList            :: Positions -> [Position]
toAscList            = IS.toAscList . unPS

-- | Constructs Positions from a list of 'Position's.
fromList             :: [Position] -> Positions
-- fromList             = PS . IS.unions . map IS.cacheAt
fromList             = PS . IS.fromList

-- | Number of 'Position's.
size                 :: Positions -> Int
size                 = IS.size . unPS

-- | The union of two 'Positions'.
union                :: Positions -> Positions -> Positions
union s1 s2          = PS $ (unPS s1) `IS.union` (unPS s2)

-- | The union of two 'Positions'.
intersection         :: Positions -> Positions -> Positions
intersection s1 s2   = PS $ (unPS s1) `IS.intersection` (unPS s2)

-- | The union of two 'Positions'.
difference           :: Positions -> Positions -> Positions
difference s1 s2     = PS $ (unPS s1) `IS.difference` (unPS s2)

-- | A fold over Positions
foldr                :: (Position -> r -> r) -> r -> Positions -> r
foldr op e           = IS.foldr op e . unPS

-- | intersection with a "shifted" 2. set with elements decremented by a displacement @d@
-- before the element test
--
-- useful when searching for sequences of words (phrases)

intersectionWithDispl :: Int -> Positions -> Positions -> Positions
intersectionWithDispl d (PS s1) (PS s2)
    = PS $ IS.filter member' s1
      where
        member' i = (i + d) `IS.member` s2

-- | intersction with "fuzzy" element test. All elements @e1@ for which an element @e2@ in @s2@
-- is found with @e2 - e1 `elem` [lb..ub]@ remain in set @s1@.
--
-- Useful for context search with sequences of words.
-- This generatizes 'intersectionWithDispl'
--
-- Law: @intersectionWithIntervall d d == intersectionWithDispl d@

intersectionWithIntervall :: Int -> Int -> Positions -> Positions -> Positions
intersectionWithIntervall lb ub (PS s1) (PS s2)
    = PS $ IS.filter member' s1
    where
      member' i = minElem <= i + ub
          where
            (_ls, gt) = IS.split  (i + lb - 1) s2
            minElem   = fromMaybe (i + ub + 1) (IS.minimum gt)

-- ------------------------------------------------------------
