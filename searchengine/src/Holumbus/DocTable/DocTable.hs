{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.DocTable.DocTable
where

import           Prelude                          hiding (filter, lookup, map,
                                                   null)
import qualified Prelude                          as P

import           Data.Maybe                       (catMaybes)
import           Data.Set                         (Set)
import qualified Data.Set                         as S

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap   (DocIdMap (..), DocIdSet,
                                                   toDocIdSet)
import           Holumbus.Index.Common.Document   (DocumentWrapper)

-- ----------------------------------------------------------------------------

-- | The doc-table data type which contains all functions used on the implementation.
--   The type parameter @i@ is the implementation.
class (DocumentWrapper (DValue i)) => DocTable i where
    type DValue i :: *

    -- | Test whether the doc table is empty.
    null            :: i -> Bool

    -- | Returns the number of unique documents in the table.
    size            :: i -> Int

    -- | Lookup a document by its ID.
    lookup          :: (Monad m, Functor m) => i -> DocId -> m (DValue i)

    -- | Lookup the 'DocId' of a document by an 'URI'.
    lookupByURI     :: (Monad m, Functor m) => i -> URI -> m DocId

    -- | Union of two disjoint document tables. It is assumed, that the
    --   DocIds and the document 'URI's of both indexes are disjoint.
    union           :: i -> i -> i

    -- | Test whether the 'DocId's of both tables are disjoint.
    disjoint        :: i -> i -> Bool

    -- | Insert a document into the table. Returns a tuple of the 'DocId' for that document and the
    -- new table. If a document with the same 'URI' is already present, its id will be returned
    -- and the table is returned unchanged.
    insert          :: i -> DValue i -> (DocId, i)

    -- | Update a document with a certain 'DocId'.
    update          :: i -> DocId -> DValue i -> i

    -- | Update a document by 'DocId' with the result of the provided function.
    adjust          :: (DValue i -> DValue i) -> DocId -> i -> i
    adjust f did d
        = maybe d (update d did . f) $ lookup d did

    -- | Update a document by 'URI' with the result of the provided function.
    adjustByURI     :: (DValue i -> DValue i) ->  URI -> i -> i
    adjustByURI f uri d
        = maybe d (flip (adjust f) d) $ lookupByURI d uri

    -- | Removes the document with the specified 'DocId' from the table.
    delete          :: i -> DocId -> i

    -- | Removes the document with the specified 'URI' from the table.
    deleteByURI     :: i -> URI -> i
    deleteByURI ds u
        = maybe ds (delete ds) (lookupByURI ds u)
    -- | Deletes a set of documentss by 'DocId' from the table.
    difference      :: DocIdSet -> i -> i

    -- | Deletes a set of documents by 'URI' from the table.
    differenceByURI :: Set URI -> i -> i
    differenceByURI uris d
        = difference ids d
        where
        ids = toDocIdSet .  catMaybes . S.toList . S.map (lookupByURI d) $ uris

    -- | Update documents (through mapping over all documents).
    map             :: (DValue i -> DValue i) -> i -> i

    -- | Filters all documents that satisfy the predicate.
    filter          :: (DValue i -> Bool) -> i -> i

    -- | Convert document table to a single map
    toMap           :: i -> DocIdMap (DValue i)

    -- | Edit 'DocId's.
    mapKeys         :: (DocId -> DocId) -> i -> i
