{-# LANGUAGE Rank2Types #-}
module Holumbus.DocTable.DocTable
where

import           Prelude                          hiding (null, filter, map)
import           Control.Arrow                    (second)

import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Maybe                       (isJust, fromJust)

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap   (DocIdMap)
import qualified Holumbus.Index.Common.DocIdMap   as DM


-- ----------------------------------------------------------------------------
--
-- external interface

-- | Test whether the doc table is empty.
null                          :: DocTable i e -> Bool
null                          = _null

-- | Returns the number of unique documents in the table.
size                          :: DocTable i e -> Int
size                          = _size

-- | Lookup a document by its id.
lookupById                    :: (Monad m, Functor m) => DocTable i e -> DocId -> m e
lookupById                    = _lookupById

-- | Lookup the id of a document by an URI.
lookupByURI                   :: (Monad m, Functor m) => DocTable i e -> URI -> m DocId
lookupByURI                   = _lookupByURI

-- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
-- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
-- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds
union                         :: DocTable i e -> DocTable i e -> DocTable i e
union                         = _union

-- | Test whether the doc ids of both tables are disjoint.
disjoint                      :: DocTable i e -> DocTable i e -> Bool
disjoint                      = _disjoint

-- | Insert a document into the table. Returns a tuple of the id for that document and the
-- new table. If a document with the same URI is already present, its id will be returned
-- and the table is returned unchanged.
insert                        :: DocTable i e -> e -> (DocId, DocTable i e)
insert                        = _insert

-- | Update a document with a certain DocId.
update                        :: DocTable i e -> DocId -> e -> DocTable i e
update                        = _update

-- | Modify a document.
modify                        :: (e -> e) -> DocId -> DocTable i e -> DocTable i e
modify f did d                = maybe d (update d did . f) $lookupById d did

-- | Modify a document by URI.
modifyByURI                   :: (e -> e) -> DocTable i e -> URI -> DocTable i e
modifyByURI f d uri           = maybe d (flip (modify f) d) $ lookupByURI d uri

-- | Removes the document with the specified id from the table.
deleteById                    :: DocTable i e -> DocId -> DocTable i e
deleteById                    = _deleteById

-- | Removes the document with the specified URI from the table.
deleteByURI                   :: DocTable i e -> URI -> DocTable i e
deleteByURI ds u              = maybe ds (deleteById ds) (lookupByURI ds u)

-- | Deletes a set of Docs by Id from the table.
differenceById                :: Set DocId -> DocTable i e -> DocTable i e
differenceById                = flip _differenceById

-- | Deletes a set of Docs by URI from the table.
differenceByURI               :: Set URI -> DocTable i e -> DocTable i e
differenceByURI uris d        = differenceById ids d
    where
    ids = S.map (fromJust) . S.filter (isJust) .  S.map (lookupByURI d) $ uris

-- | Update documents (through mapping over all documents).
map                           :: (e -> e) -> DocTable i e -> DocTable i e
map                           = flip _map

-- | Filters all documents that satisfy the predicate.
filter                        :: (e -> Bool) -> DocTable i e -> DocTable i e
filter                        = flip _filter

-- | Convert document table to a single map
toMap                         :: DocTable i e -> DocIdMap e
toMap                         = _toMap

-- | Edit document ids
editDocIds                    :: (DocId -> DocId) -> DocTable i e -> DocTable i e
editDocIds                    = flip _editDocIds

-- | The doctable implementation.
impl                          :: DocTable i e -> i
impl                          = _impl

-- ----------------------------------------------------------------------------

data DocTable i e = Dt
    {
    -- | Test whether the doc table is empty.
      _null                        :: Bool

    -- | Returns the number of unique documents in the table.
    , _size                          :: Int

    -- | Lookup a document by its id.
    , _lookupById                    :: (Monad m, Functor m) => DocId -> m e

    -- | Lookup the id of a document by an URI.
    , _lookupByURI                   :: (Monad m, Functor m) => URI -> m DocId

    -- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds
    , _union                         :: DocTable i e -> DocTable i e

    -- | Test whether the doc ids of both tables are disjoint.
    , _disjoint                      :: DocTable i e -> Bool

    -- | Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.
    , _insert                        :: e -> (DocId, DocTable i e)

    -- | Update a document with a certain DocId.
    , _update                        :: DocId -> e -> DocTable i e

    -- | Removes the document with the specified id from the table.
    , _deleteById                    :: DocId -> DocTable i e

    -- | Deletes a set of Docs by Id from the table.
    , _differenceById                :: Set DocId -> DocTable i e

    -- | Update documents (through mapping over all documents).
    , _map                           :: (e -> e) -> DocTable i e

    -- | Filters all documents that satisfy the predicate.
    , _filter                        :: (e -> Bool) -> DocTable i e

    -- | Convert document table to a single map
    , _toMap                         :: DocIdMap e

    -- | Edit document ids
    , _editDocIds                    :: (DocId -> DocId) -> DocTable i e

    -- | The doctable implementation.
    , _impl                          :: i
    }


newConvValueDocTable :: (a -> v) -> (v -> a) -> DocTable i a -> DocTable (DocTable i a) v
newConvValueDocTable from to i =
    Dt
    {
      _null                          = null i
    , _size                          = size i
    , _lookupById                    = fmap from . lookupById i
    , _lookupByURI                   = lookupByURI i
    , _union                         = \dt2 -> cv $ union i (impl dt2)
    , _disjoint                      = \dt2 -> disjoint i (impl dt2)
    , _insert                        = \e -> second cv $ insert i (to e)
    , _update                        = \did e -> cv $ update i did (to e)
    , _deleteById                    = cv . deleteById i
    , _differenceById                = cv . flip differenceById i
    , _map                           = \f -> cv $ map (to . f . from) i
    , _filter                        = \f -> cv $ filter (f . from) i
    , _toMap                         = DM.map from (toMap i)
    , _editDocIds                    = \f -> cv $ editDocIds f i
    , _impl                          = i
    }
    where
      cv = newConvValueDocTable from to
