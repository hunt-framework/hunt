-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Common.Occurrences
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  Occurrences of words within the index.
  A word occurs in document at specific locations.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Occurrences
where

import           Prelude                hiding (subtract)

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap   (DocIdMap)
import           Hunt.Common.DocIdSet   (DocIdSet)
import qualified Hunt.Common.DocIdMap   as DM
import           Hunt.Common.Positions  (Positions)
import qualified Hunt.Common.Positions  as Pos
import qualified Hunt.Common.DocIdMap.Packed as DMP

-- ------------------------------------------------------------

-- | The occurrences of words in documents.
--   A mapping from document ids to the positions in the document.

type Occurrences        = DocIdMap Positions

type DenseOccurrences   = DMP.DocIdMap Positions

-- ------------------------------------------------------------

-- | Create an empty set of positions.
empty                   :: Occurrences
empty                   = DM.empty

-- | Create Occurrences from a 'DocIdSet'
--   Since the 'DocIdSet' contains no position information, we
--   assume position one for each 'DocId'
fromDocIdSet            :: DocIdSet -> Occurrences
fromDocIdSet s          = DM.fromDocIdSet (\_ -> Pos.singleton 1) s

-- | Create a single dcid set with a single position.
singleton               :: DocId -> Position -> Occurrences
singleton d p           = singleton' d [p]

-- | Create a single dcid set with a set of.
singleton'              :: DocId -> [Position] -> Occurrences
singleton' d ps         = DM.insert d (Pos.fromList ps) DM.empty

-- | Test on empty set of positions.
null                    :: Occurrences -> Bool
null                    = DM.null

-- | Determine the number of positions in a set of occurrences.
size                    :: Occurrences -> Int
size                    = DM.foldr ((+) . Pos.size) 0

-- | Add a position to occurrences.
insert                  :: DocId -> Position -> Occurrences -> Occurrences
insert d p              = DM.insertWith Pos.union d (Pos.singleton p)

-- | Add multiple positions to occurrences
insert'                 :: DocId -> Positions -> Occurrences -> Occurrences
insert' d ps occs       = Pos.foldr (insert d) occs ps

-- | Remove a position from occurrences.
deleteOccurrence        :: DocId -> Position -> Occurrences -> Occurrences
deleteOccurrence d p    = subtract (DM.singleton d (Pos.singleton p))

-- | Delete a document (by 'DocId') from occurrences.
delete                  :: DocId -> Occurrences -> Occurrences
delete                  = DM.delete

-- | Changes the DocIDs of the occurrences.
update                  :: (DocId -> DocId) -> Occurrences -> Occurrences
update f                = DM.foldrWithKey
                          (\ d ps res -> DM.insertWith Pos.union (f d) ps res) empty

-- | Merge two occurrences.
merge                   :: Occurrences -> Occurrences -> Occurrences
merge                   = DM.unionWith Pos.union

-- | Merge occurrences
merges                  :: [Occurrences] -> Occurrences
merges                  = DM.unionsWith Pos.union

-- | Difference of occurrences.
difference              :: Occurrences -> Occurrences -> Occurrences
difference              = DM.difference

-- | Remove Set of DocIds from Occurrences
diffWithSet             :: Occurrences -> DocIdSet -> Occurrences
diffWithSet             = DM.diffWithSet

-- | Subtract occurrences from some other occurrences.
subtract                :: Occurrences -> Occurrences -> Occurrences
subtract                = DM.differenceWith subtractPositions
  where
  subtractPositions p1 p2
    = if Pos.null diffPos
        then Nothing
        else Just diffPos
    where
    diffPos = Pos.difference p1 p2

intersectOccurrences    :: (Positions -> Positions -> Positions) ->
                           Occurrences -> Occurrences -> Occurrences
intersectOccurrences pf os1 os2
    = DM.filter (not . Pos.null) $ DM.intersectionWith pf os1 os2

-- ------------------------------------------------------------
