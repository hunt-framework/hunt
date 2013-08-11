{-# LANGUAGE Rank2Types #-}
module Holumbus.DocTable.DocTable
where

import           Prelude                          hiding (null)
import           Control.Arrow                    (second)

import           Data.Set                         (Set)

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
removeById                    :: DocTable i e -> DocId -> DocTable i e
removeById                    = _removeById

-- | Removes the document with the specified URI from the table.
removeByURI                   :: DocTable i e -> URI -> DocTable i e
removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)

-- | Deletes a set of Docs by Id from the table.
deleteById                    :: Set DocId -> DocTable i e -> DocTable i e
deleteById                    = flip _deleteById

{-
-- | Deletes a set of Docs by Uri from the table. Uris that are not in the docTable are ignored.
deleteByUri                   :: Set URI -> d -> d
deleteByUri us ds             = deleteById idSet ds
  where
  idSet = catMaybesSet . S.map (lookupByURI ds) $ us
-}

-- | Update documents (through mapping over all documents).
updateDocuments               :: (e -> e) -> DocTable i e -> DocTable i e
updateDocuments               = flip _updateDocuments

filterDocuments               :: (e -> Bool) -> DocTable i e -> DocTable i e
filterDocuments               = flip _filterDocuments

-- XXX: impl.
-- | Create a document table from a single map.
--fromMap                       :: DocIdMap Document -> DocTable i e

-- | Convert document table to a single map
toMap                         :: DocTable i e -> DocIdMap e
toMap                         = _toMap

-- | Edit document ids
editDocIds                    :: (DocId -> DocId) -> DocTable i e -> DocTable i e
-- editDocIds f                  = fromMap . DM.foldWithKey (DM.insert . f) DM.empty . toMap
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
    , _removeById                    :: DocId -> DocTable i e

    -- | Removes the document with the specified URI from the table.
    , _removeByURI                   :: URI -> DocTable i e

    -- | Deletes a set of Docs by Id from the table.
    , _deleteById                    :: Set DocId -> DocTable i e

    {-
    -- | Deletes a set of Docs by Uri from the table. Uris that are not in the docTable are ignored.
    deleteByUri                   :: Set URI -> d -> d
    deleteByUri us ds             = deleteById idSet ds
      where
      idSet = catMaybesSet . S.map (lookupByURI ds) $ us
    -}

    -- | Update documents (through mapping over all documents).
    , _updateDocuments               :: (e -> e) -> DocTable i e

    , _filterDocuments               :: (e -> Bool) -> DocTable i e

    -- XXX: impl.
    -- | Create a document table from a single map.
    --, _fromMap                       :: DocIdMap Document -> DocTable i

    -- | Convert document table to a single map
    , _toMap                         :: DocIdMap e

    -- | Edit document ids
    , _editDocIds                    :: (DocId -> DocId) -> DocTable i e
    -- editDocIds f                  = fromMap . DM.foldWithKey (DM.insert . f) DM.empty . toMap

    -- | The doctable implementation.
    , _impl                          :: i
    }


newConvValueDocTable :: (a -> v) -> (v -> a) -> DocTable i a -> DocTable (DocTable i a) v
newConvValueDocTable from to i =
    Dt
    {
    -- | Test whether the doc table is empty.
      _null                          = null i

    -- | Returns the number of unique documents in the table.
    , _size                          = size i

    -- | Lookup a document by its id.
    , _lookupById                    = fmap from . lookupById i

    -- | Lookup the id of a document by an URI.
    , _lookupByURI                   = lookupByURI i

    -- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

    , _union                         = \dt2 -> cv $ union i (impl dt2)
    -- unionDocs dt1                 = DM.fold addDoc dt1 . toMap
    --    where
    --    addDoc d dt               = snd . insertDoc dt $ d

    -- | Test whether the doc ids of both tables are disjoint.
    , _disjoint                      = \dt2 -> disjoint i (impl dt2)

    -- | Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.
    , _insert                        = \e -> second cv $ insert i (to e)

    -- | Update a document with a certain DocId.
    , _update                        = \did e -> cv $ update i did (to e)

    -- | Removes the document with the specified id from the table.
    , _removeById                    = cv . removeById i

    -- | Removes the document with the specified URI from the table.
    , _removeByURI                   = cv . removeByURI i
    -- removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)

    -- | Deletes a set of Docs by Id from the table.
    , _deleteById                    = cv . flip deleteById i

    {-
    -- | Deletes a set of Docs by Uri from the table. Uris that are not in the docTable are ignored.
    deleteByUri                   :: Set URI -> d -> d
    deleteByUri us ds             = deleteById idSet ds
      where
      idSet = catMaybesSet . S.map (lookupByURI ds) $ us
    -}

    -- | Update documents (through mapping over all documents).
    , _updateDocuments               = \f -> cv $ updateDocuments (to . f . from) i

    , _filterDocuments               = \f -> cv $ filterDocuments (f . from) i

    -- | Create a document table from a single map.
    --, _fromMap                       :: DocIdMap Document -> DocTable i

    -- | Convert document table to a single map
    , _toMap                         = DM.map from (toMap i)

    -- | Edit document ids
    , _editDocIds                    = \f -> cv $ editDocIds f i

    -- | The doctable implementation.
    , _impl                          = i
    }
    where
      cv = newConvValueDocTable from to
