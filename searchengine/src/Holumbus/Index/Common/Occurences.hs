{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.Occurences
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The Occurences data type

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.Occurences
where

import           Control.DeepSeq

import           Data.Binary                      (Binary (..))
import qualified Data.Binary                      as B

import qualified Data.EnumSet                     as IS

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap   as DM


-- ------------------------------------------------------------

-- | The occurrences in a number of documents.
-- A mapping from document ids to the positions in the document.
type Occurrences        = DocIdMap Positions

-- | Create an empty set of positions.
emptyOccurrences        :: Occurrences
emptyOccurrences        = empty

-- | Create an empty set of positions.
singletonOccurrence     :: DocId -> Position -> Occurrences
singletonOccurrence d p = insertOccurrence d p empty

-- | Test on empty set of positions.
nullOccurrences         :: Occurrences -> Bool
nullOccurrences         = DM.null

-- | Determine the number of positions in a set of occurrences.
sizeOccurrences         :: Occurrences -> Int
sizeOccurrences         = DM.foldr ((+) . IS.size) 0

-- | Add a position to occurrences.
insertOccurrence        :: DocId -> Position -> Occurrences -> Occurrences
insertOccurrence d p    = insertWith IS.union d (singletonPos p)

-- | Remove a position from occurrences.
deleteOccurrence        :: DocId -> Position -> Occurrences -> Occurrences
deleteOccurrence d p    = substractOccurrences (singleton d (singletonPos p))

-- | Delete a document (by 'DocId') from occurrences.
delete                  :: DocId -> Occurrences -> Occurrences
delete                  = DM.delete

-- | Changes the DocIDs of the occurrences.
updateOccurrences       :: (DocId -> DocId) -> Occurrences -> Occurrences
updateOccurrences f     = foldrWithKey
                          (\ d ps res -> insertWith IS.union (f d) ps res) emptyOccurrences

-- | Merge two occurrences.
mergeOccurrences        :: Occurrences -> Occurrences -> Occurrences
mergeOccurrences        = unionWith IS.union

-- | Difference of occurrences.
diffOccurrences         :: Occurrences -> Occurrences -> Occurrences
diffOccurrences          = difference

-- | Substract occurrences from some other occurrences.
substractOccurrences    :: Occurrences -> Occurrences -> Occurrences
substractOccurrences    = differenceWith substractPositions
  where
  substractPositions p1 p2
                        = if IS.null diffPos
                          then Nothing
                          else Just diffPos
      where
      diffPos                   = IS.difference p1 p2

-- ------------------------------------------------------------

-- | The positions of the word in the document.
type Positions                  = IS.EnumSet Position

-- | Empty positions.
emptyPos                :: Positions
emptyPos                = IS.empty

-- | Positions with one element.
singletonPos            :: Position -> Positions
singletonPos            = IS.singleton

-- Whether the 'Position' is part of 'Positions'.
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

-- | The XML pickler for a set of positions.
instance (NFData v, Enum v) => NFData (IS.EnumSet v) where
    rnf                   = rnf . IS.toList

instance (Binary v, Enum v) => Binary (IS.EnumSet v) where
    put                   = B.put . IS.toList
    get                   = B.get >>= return . IS.fromList

-- ------------------------------------------------------------

