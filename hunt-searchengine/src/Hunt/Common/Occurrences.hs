-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.Occurences
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The Occurences data type

-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Occurrences
where

import           Prelude                     hiding (subtract)

import qualified Data.IntSet                 as IS

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap    (DocIdMap)
import qualified Hunt.Common.DocIdMap    as DM
import           Hunt.Common.Positions   (Positions)
import qualified Hunt.Common.Positions   as Pos

-- ------------------------------------------------------------

-- | The occurrences in a number of documents.
-- A mapping from document ids to the positions in the document.
type Occurrences        = DocIdMap Positions

-- | Create an empty set of positions.
empty                   :: Occurrences
empty                   = DM.empty

-- | Create an empty set of positions.
singleton               :: DocId -> Position -> Occurrences
singleton d p           = insert d p DM.empty

-- | Test on empty set of positions.
null                    :: Occurrences -> Bool
null                    = DM.null

-- | Determine the number of positions in a set of occurrences.
size                    :: Occurrences -> Int
size                    = DM.foldr ((+) . IS.size) 0

-- | Add a position to occurrences.
insert                  :: DocId -> Position -> Occurrences -> Occurrences
insert d p              = DM.insertWith IS.union d (Pos.singleton p)

-- | Add multiple positions to occurrences
insert'                 :: DocId -> Positions -> Occurrences -> Occurrences
insert' d ps occs       = IS.foldr (insert d) occs ps

-- | Remove a position from occurrences.
deleteOccurrence        :: DocId -> Position -> Occurrences -> Occurrences
deleteOccurrence d p    = subtract (DM.singleton d (Pos.singleton p))

-- | Delete a document (by 'DocId') from occurrences.
delete                  :: DocId -> Occurrences -> Occurrences
delete                  = DM.delete

-- | Changes the DocIDs of the occurrences.
update                  :: (DocId -> DocId) -> Occurrences -> Occurrences
update f                = DM.foldrWithKey
                          (\ d ps res -> DM.insertWith IS.union (f d) ps res) empty

-- | Merge two occurrences.
merge                   :: Occurrences -> Occurrences -> Occurrences
merge                   = DM.unionWith Pos.union

-- | Difference of occurrences.
difference              :: Occurrences -> Occurrences -> Occurrences
difference              = DM.difference

-- | Subtract occurrences from some other occurrences.
subtract                :: Occurrences -> Occurrences -> Occurrences
subtract                = DM.differenceWith subtractPositions
  where
  subtractPositions p1 p2
    = if IS.null diffPos
        then Nothing
        else Just diffPos
    where
    diffPos = IS.difference p1 p2

-- ------------------------------------------------------------


