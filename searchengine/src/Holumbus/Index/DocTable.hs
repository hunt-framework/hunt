{-# LANGUAGE Rank2Types #-}
module Holumbus.Index.DocTable
where

import           Data.Set                         (Set)

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap   (DocIdMap)
import           Holumbus.Index.Common.Document


-- ----------------------------------------------------------------------------
--
-- external interface

nullDocs                      :: DocTable i -> Bool
nullDocs                      = _nullDocs

-- | Returns the number of unique documents in the table.
sizeDocs                      :: DocTable i -> Int
sizeDocs                      = _sizeDocs

-- | Lookup a document by its id.
lookupById                    :: Monad m => DocTable i -> DocId -> m Document
lookupById                    = _lookupById

-- | Lookup the id of a document by an URI.
lookupByURI                   :: Monad m => DocTable i -> URI -> m DocId
lookupByURI                   = _lookupByURI

-- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
-- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
-- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

-- XXX: field or def. impl?
unionDocs                     :: DocTable i -> DocTable i -> DocTable i
unionDocs                     = _unionDocs
--unionDocs dt1                 = DM.fold addDoc dt1 . toMap
--   where
--   addDoc d dt                = snd . insertDoc dt $ d

-- | Test whether the doc ids of both tables are disjoint.
disjointDocs                  :: DocTable i -> DocTable i -> Bool
disjointDocs                  = _disjointDocs

-- | Return an empty document table.
-- makeEmpty                     :: DocTable i -> DocTable i
-- makeEmpty                     = _makeEmpty

-- | Insert a document into the table. Returns a tuple of the id for that document and the
-- new table. If a document with the same URI is already present, its id will be returned
-- and the table is returned unchanged.
insertDoc                     :: DocTable i -> Document -> (DocId, DocTable i)
insertDoc                     = _insertDoc

-- | Update a document with a certain DocId.
updateDoc                     :: DocTable i -> DocId -> Document -> DocTable i
updateDoc                     = _updateDoc

-- XXX: reverse order of arguments?
-- | Removes the document with the specified id from the table.
removeById                    :: DocTable i -> DocId -> DocTable i
removeById                    = _removeById

-- XXX: field or def. impl?
-- | Removes the document with the specified URI from the table.
removeByURI                   :: DocTable i -> URI -> DocTable i
removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)
--removeByURI                   = _removeByURI

-- | Deletes a set of Docs by Id from the table.
deleteById                    :: Set DocId -> DocTable i -> DocTable i
deleteById                    = flip _deleteById

{-
-- | Deletes a set of Docs by Uri from the table. Uris that are not in the docTable are ignored.
deleteByUri                   :: Set URI -> d -> d
deleteByUri us ds             = deleteById idSet ds
  where
  idSet = catMaybesSet . S.map (lookupByURI ds) $ us
-}

-- | Update documents (through mapping over all documents).
updateDocuments               :: (Document -> Document) -> DocTable i -> DocTable i
updateDocuments               = flip _updateDocuments

filterDocuments               :: (Document -> Bool) -> DocTable i -> DocTable i
filterDocuments               = flip _filterDocuments

-- | Create a document table from a single map.
--fromMap                       :: DocIdMap Document -> DocTable i

-- | Convert document table to a single map
toMap                         :: DocTable i -> DocIdMap Document
toMap                         = _toMap

-- | Edit document ids
editDocIds                    :: (DocId -> DocId) -> DocTable i -> DocTable i
-- editDocIds f                  = fromMap . DM.foldWithKey (DM.insert . f) DM.empty . toMap
editDocIds                    = flip _editDocIds

-- | The doctable implementation.
impl                          :: DocTable i -> i
impl                          = _impl

-- ----------------------------------------------------------------------------

data DocTable i = Dt
    {
        _nullDocs                      :: Bool
      -- nullDocs                      = (== 0) . sizeDocs

    -- | Returns the number of unique documents in the table.
    , _sizeDocs                      :: Int

    -- | Lookup a document by its id.
    , _lookupById                    :: Monad m => DocId -> m Document

    -- | Lookup the id of a document by an URI.
    , _lookupByURI                   :: Monad m => URI -> m DocId

    -- | Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds

    , _unionDocs                     :: DocTable i -> DocTable i
    -- unionDocs dt1                 = DM.fold addDoc dt1 . toMap
    --    where
    --    addDoc d dt               = snd . insertDoc dt $ d

    -- | Test whether the doc ids of both tables are disjoint.
    , _disjointDocs                  :: DocTable i -> Bool

    -- | Return an empty document table.
    -- , _makeEmpty                     :: DocTable i

    -- | Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.

    , _insertDoc                     :: Document -> (DocId, DocTable i)

    -- | Update a document with a certain DocId.
    , _updateDoc                     :: DocId -> Document -> DocTable i

    -- XXX: reverse order of arguments?
    -- | Removes the document with the specified id from the table.
    , _removeById                    :: DocId -> DocTable i

    -- | Removes the document with the specified URI from the table.
    , _removeByURI                   :: URI -> DocTable i
    -- removeByURI ds u              = maybe ds (removeById ds) (lookupByURI ds u)

    -- | Deletes a set of Docs by Id from the table.
    , _deleteById                    :: Set DocId -> DocTable i

    {-
    -- | Deletes a set of Docs by Uri from the table. Uris that are not in the docTable are ignored.
    deleteByUri                   :: Set URI -> d -> d
    deleteByUri us ds             = deleteById idSet ds
      where
      idSet = catMaybesSet . S.map (lookupByURI ds) $ us
    -}

    -- | Update documents (through mapping over all documents).
    , _updateDocuments               :: (Document -> Document) -> DocTable i

    , _filterDocuments               :: (Document -> Bool) -> DocTable i

    -- | Create a document table from a single map.
    --, _fromMap                       :: DocIdMap Document -> DocTable i

    -- | Convert document table to a single map
    , _toMap                         :: DocIdMap Document

    -- | Edit document ids
    , _editDocIds                    :: (DocId -> DocId) -> DocTable i
    -- editDocIds f                  = fromMap . DM.foldWithKey (DM.insert . f) DM.empty . toMap

    -- | The doctable implementation.
    , _impl                          :: i
    }