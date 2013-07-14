{-# LANGUAGE Rank2Types #-}
module Holumbus.Index.DocTable
where

import           Control.Arrow                    (second)

import           Data.Set                         (Set)

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap   as DM


-- ----------------------------------------------------------------------------
--
-- external interface

nullDocs                      :: DocTable i e -> Bool
nullDocs                      = _nullDocs

-- | Returns the number of unique documents in the table.
sizeDocs                      :: DocTable i e -> Int
sizeDocs                      = _sizeDocs

-- | Lookup a document by its id.
lookupById                    :: (Monad m, Functor m) => DocTable i e -> DocId -> m e
lookupById                    = _lookupById

-- | Lookup the id of a document by an URI.
lookupByURI                   :: (Monad m, Functor m) => DocTable i e -> URI -> m DocId
lookupByURI                   = _lookupByURI

-- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
-- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
-- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

-- XXX: field or def. impl?
unionDocs                     :: DocTable i e -> DocTable i e -> DocTable i e
unionDocs                     = _unionDocs
--unionDocs dt1                 = DM.fold addDoc dt1 . toMap
--   where
--   addDoc d dt                = snd . insertDoc dt $ d

-- | Test whether the doc ids of both tables are disjoint.
disjointDocs                  :: DocTable i e -> DocTable i e -> Bool
disjointDocs                  = _disjointDocs

-- | Return an empty document table.
-- makeEmpty                     :: DocTable i e -> DocTable i e
-- makeEmpty                     = _makeEmpty

-- | Insert a document into the table. Returns a tuple of the id for that document and the
-- new table. If a document with the same URI is already present, its id will be returned
-- and the table is returned unchanged.
insertDoc                     :: DocTable i e -> e -> (DocId, DocTable i e)
insertDoc                     = _insertDoc

-- | Update a document with a certain DocId.
updateDoc                     :: DocTable i e -> DocId -> e -> DocTable i e
updateDoc                     = _updateDoc

-- XXX: reverse order of arguments?
-- | Removes the document with the specified id from the table.
removeById                    :: DocTable i e -> DocId -> DocTable i e
removeById                    = _removeById

-- XXX: field or def. impl?
-- | Removes the document with the specified URI from the table.
removeByURI                   :: DocTable i e -> URI -> DocTable i e
removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)
--removeByURI                   = _removeByURI

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
        _nullDocs                      :: Bool
      -- nullDocs                      = (== 0) . sizeDocs

    -- | Returns the number of unique documents in the table.
    , _sizeDocs                      :: Int

    -- | Lookup a document by its id.
    , _lookupById                    :: (Monad m, Functor m) => DocId -> m e

    -- | Lookup the id of a document by an URI.
    , _lookupByURI                   :: (Monad m, Functor m) => URI -> m DocId

    -- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

    , _unionDocs                     :: DocTable i e -> DocTable i e
    -- unionDocs dt1                 = DM.fold addDoc dt1 . toMap
    --    where
    --    addDoc d dt               = snd . insertDoc dt $ d

    -- | Test whether the doc ids of both tables are disjoint.
    , _disjointDocs                  :: DocTable i e -> Bool

    -- | Return an empty document table.
    -- , _makeEmpty                     :: DocTable i

    -- | Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.

    , _insertDoc                     :: e -> (DocId, DocTable i e)

    -- | Update a document with a certain DocId.
    , _updateDoc                     :: DocId -> e -> DocTable i e

    -- XXX: reverse order of arguments?
    -- | Removes the document with the specified id from the table.
    , _removeById                    :: DocId -> DocTable i e

    -- | Removes the document with the specified URI from the table.
    , _removeByURI                   :: URI -> DocTable i e
    -- removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)

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
      _nullDocs                      = nullDocs i

    -- | Returns the number of unique documents in the table.
    , _sizeDocs                      = sizeDocs i

    -- | Lookup a document by its id.
    , _lookupById                    = fmap from . lookupById i

    -- | Lookup the id of a document by an URI.
    , _lookupByURI                   = lookupByURI i

    -- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

    , _unionDocs                     = \dt2 -> cv $ unionDocs i (impl dt2)
    -- unionDocs dt1                 = DM.fold addDoc dt1 . toMap
    --    where
    --    addDoc d dt               = snd . insertDoc dt $ d

    -- | Test whether the doc ids of both tables are disjoint.
    , _disjointDocs                  = \dt2 -> disjointDocs i (impl dt2)

    -- | Return an empty document table.
    -- , _makeEmpty                     = undefined

    -- | Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.

    , _insertDoc                     = \e -> second cv $ insertDoc i (to e)

    -- | Update a document with a certain DocId.
    , _updateDoc                     = \did e -> cv $ updateDoc i did (to e)

    -- XXX: reverse order of arguments?
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
    -- editDocIds f                  = fromMap . DM.foldWithKey (DM.insert . f) DM.empty . toMap

    -- | The doctable implementation.
    , _impl                          = i
    }
    where
      cv = newConvValueDocTable from to