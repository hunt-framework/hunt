{-# LANGUAGE Rank2Types #-}
module Holumbus.DocTable.DocTable
where

import           Prelude                          hiding (filter, lookup, map,
                                                   null)
import qualified Prelude                          as P

import           Control.DeepSeq

import           Data.Maybe                       (fromJust, isJust)
import           Data.Set                         (Set)
import qualified Data.Set                         as S

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap   (DocIdMap (..), DocIdSet,
                                                   toDocIdSet)

-- ----------------------------------------------------------------------------
--
-- external interface

-- | Test whether the doc table is empty.
null                          :: DocTable i e -> Bool
null                          = _null

-- | Returns the number of unique documents in the table.
size                          :: DocTable i e -> Int
size                          = _size

-- | Lookup a document by its ID.
lookup                        :: (Monad m, Functor m) => DocTable i e -> DocId -> m e
lookup                        = _lookup

-- | Lookup the 'DocId' of a document by an 'URI'.
lookupByURI                   :: (Monad m, Functor m) => DocTable i e -> URI -> m DocId
lookupByURI                   = _lookupByURI

-- | Union of two disjoint document tables. It is assumed, that the DocIds and the document 'URI's
-- of both indexes are disjoint.
union                         :: DocTable i e -> DocTable i e -> DocTable i e
union                         = _union

-- | Test whether the 'DocId's of both tables are disjoint.
disjoint                      :: DocTable i e -> DocTable i e -> Bool
disjoint                      = _disjoint

-- | Insert a document into the table. Returns a tuple of the 'DocId' for that document and the
-- new table. If a document with the same 'URI' is already present, its id will be returned
-- and the table is returned unchanged.
insert                        :: DocTable i e -> e -> (DocId, DocTable i e)
insert                        = _insert

-- | Update a document with a certain 'DocId'.
update                        :: DocTable i e -> DocId -> e -> DocTable i e
update                        = _update

-- | Update a document by 'DocId' with the result of the provided function.
adjust                        :: (e -> e) -> DocId -> DocTable i e -> DocTable i e
adjust f did d                = maybe d (update d did . f) $ lookup d did

-- | Update a document by 'URI' with the result of the provided function.
adjustByURI                   :: (e -> e) ->  URI -> DocTable i e -> DocTable i e
adjustByURI f uri d           = maybe d (flip (adjust f) d) $ lookupByURI d uri

-- | Removes the document with the specified 'DocId' from the table.
delete                        :: DocTable i e -> DocId -> DocTable i e
delete                        = _delete

-- | Removes the document with the specified 'URI' from the table.
deleteByURI                   :: DocTable i e -> URI -> DocTable i e
deleteByURI ds u              = maybe ds (delete ds) (lookupByURI ds u)

-- | Deletes a set of documentss by 'DocId' from the table.
difference                    :: DocIdSet -> DocTable i e -> DocTable i e
difference                    = flip _difference

-- | Deletes a set of documents by 'URI' from the table.
differenceByURI               :: Set URI -> DocTable i e -> DocTable i e
differenceByURI uris d        = difference ids d
    where
    ids = toDocIdSet .  P.map fromJust . P.filter isJust . S.toList . S.map (lookupByURI d) $ uris

-- | Update documents (through mapping over all documents).
map                           :: (e -> e) -> DocTable i e -> DocTable i e
map                           = flip _map

-- | Filters all documents that satisfy the predicate.
filter                        :: (e -> Bool) -> DocTable i e -> DocTable i e
filter                        = flip _filter

-- | Convert document table to a single map
toMap                         :: DocTable i e -> DocIdMap e
toMap                         = _toMap

-- | Edit 'DocId's.
mapKeys                       :: (DocId -> DocId) -> DocTable i e -> DocTable i e
mapKeys                       = flip _mapKeys

-- | The doctable implementation.
impl                          :: DocTable i e -> i
impl                          = _impl

-- ----------------------------------------------------------------------------

-- | The doc-table data type which contains all functions used on the implementation.
--   The type parameters are:
--
--   - @i@: the implementation
--
--   - @e@: the value/document
data DocTable i e = Dt
    {
    -- | Test whether the doc table is empty.
      _null        :: Bool

    -- | Number of unique documents in the table.
    , _size        :: Int

    -- | Lookup a document by its 'DocId'.
    , _lookup      :: (Monad m, Functor m) => DocId -> m e

    -- | Lookup the 'DocId' of a document by an 'URI'.
    , _lookupByURI :: (Monad m, Functor m) => URI -> m DocId

    -- | Union of two disjoint document tables. It is assumed, that the 'DocId's and the document 'URI's
    -- of both indexes are disjoint.
    , _union       :: DocTable i e -> DocTable i e

    -- | Test whether the 'DocId's of both tables are disjoint.
    , _disjoint    :: DocTable i e -> Bool

    -- | Insert a document into the table. Returns a tuple of the 'DocId' for that document and the
    -- new table. If a document with the same 'URI' is already present, its 'DocId' will be returned
    -- and the table is returned unchanged.
    , _insert      :: e -> (DocId, DocTable i e)

    -- | Update a document with a certain 'DocId'.
    , _update      :: DocId -> e -> DocTable i e

    -- | Removes the document with the specified 'DocId' from the table.
    , _delete      :: DocId -> DocTable i e

    -- | Deletes a set of documentss by 'DocId' from the table.
    , _difference  :: DocIdSet -> DocTable i e

    -- | Update documents (through mapping over all documents).
    , _map         :: (e -> e) -> DocTable i e

    -- | Filters all documents that satisfy the predicate.
    , _filter      :: (e -> Bool) -> DocTable i e

    -- | Convert document table to a single map
    , _toMap       :: DocIdMap e

    -- | Edit document 'DocIds's
    , _mapKeys     :: (DocId -> DocId) -> DocTable i e

    -- | The doctable implementation.
    , _impl        :: i
    }

-- ----------------------------------------------------------------------------

instance NFData (DocTable i e) where
    rnf Dt{} = rnf _impl
