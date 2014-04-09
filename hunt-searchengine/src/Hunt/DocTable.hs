{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- ----------------------------------------------------------------------------
{- |
  The document table interface.
-}
-- ----------------------------------------------------------------------------

module Hunt.DocTable
where

import           Prelude                hiding (filter, lookup, map, null)
import qualified Prelude                as P

import           Control.Applicative    (Applicative, (<$>))
import           Control.Monad

import           Data.Aeson
import           Data.Maybe             (catMaybes)
import           Data.Set               (Set)
import qualified Data.Set               as S

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap   (DocIdMap (..), DocIdSet)
import qualified Hunt.Common.DocIdMap   as DM
import           Hunt.Common.Document   (Document, DocumentWrapper (wrap, unwrap))

-- ------------------------------------------------------------

-- | The document table type class which needs to be implemented to be used by the 'Interpreter'.
--   The type parameter @i@ is the implementation.
--   The implementation must have a value type parameter.
class (DocumentWrapper (DValue i)) => DocTable i where
    -- | The value type of the document table.
    type DValue i :: *

    -- | Test whether the document table is empty.
    null            :: (Monad m) => i -> m Bool

    -- | Returns the number of unique documents in the table.
    size            :: (Monad m) => i -> m Int

    -- | Lookup a document by its ID.
    lookup          :: (Monad m) => DocId -> i -> m (Maybe (DValue i))

    -- | Lookup the 'DocId' of a document by an 'URI'.
    lookupByURI     :: (Monad m) => URI -> i -> m (Maybe DocId)

    -- | Union of two disjoint document tables. It is assumed, that the
    --   DocIds and the document 'URI's of both indexes are disjoint.
    union           :: (Monad m) => i -> i -> m i

    -- | Test whether the 'DocId's of both tables are disjoint.
    disjoint        :: (Monad m) => i -> i -> m Bool

    -- | Insert a document into the table. Returns a tuple of the 'DocId' for that document and the
    --   new table. If a document with the same 'URI' is already present, its id will be returned
    --   and the table is returned unchanged.
    insert          :: (Monad m) => DValue i -> i -> m (DocId, i)

    -- | Update a document with a certain 'DocId'.
    update          :: (Monad m) => DocId -> DValue i -> i -> m i

    -- | Update a document by 'DocId' with the result of the provided function.
    adjust          :: (Monad m) => (DValue i -> m (DValue i)) -> DocId -> i -> m i
    adjust f did d =
        maybe (return d) (upd d did <=< f) =<< lookup did d
        --maybe d (update d did . f) $ lookup d did
        where upd i docid v = update docid v i

    -- | Update a document by 'URI' with the result of the provided function.
    adjustByURI     :: (Monad m) => (DValue i -> m (DValue i)) -> URI -> i -> m i
    adjustByURI f uri d
        = maybe (return d) (flip (adjust f) d) =<< lookupByURI uri d

    -- | Removes the document with the specified 'DocId' from the table.
    delete          :: (Monad m) => DocId -> i -> m i

    -- | Removes the document with the specified 'URI' from the table.
    deleteByURI     :: (Monad m) => URI -> i -> m i
    deleteByURI u ds
        = maybe (return ds) (flip delete ds) =<< lookupByURI u ds

    -- | Deletes a set of documentss by 'DocId' from the table.
    difference      :: (Monad m) => DocIdSet -> i -> m i

    -- | Deletes a set of documents by 'URI' from the table.
    differenceByURI :: (Monad m) => Set URI -> i -> m i
    differenceByURI uris d = do -- XXX: eliminate S.toList?
        ids <- liftM (DM.toDocIdSet . catMaybes) . mapM (flip lookupByURI d) . S.toList $ uris
        difference ids d

    -- | Map a function over all values of the document table.
    map             :: (Monad m) => (DValue i -> DValue i) -> i -> m i

    -- | Filters all documents that satisfy the predicate.
    filter          :: (Monad m) => (DValue i -> Bool) -> i -> m i

    -- | Convert document table to a 'DocIdMap'.
    toMap           :: (Monad m) => i -> m (DocIdMap (DValue i))

    -- | Empty 'DocTable'.
    empty           :: i

-- ------------------------------------------------------------

-- | JSON dump of the document table.
toJSON'DocTable :: (Functor m, Monad m, Applicative m, DocTable i) => i -> m Value
toJSON'DocTable dt
    = do didm <- DM.map unwrap <$> toMap dt
         return $ toJSON didm

-- | JSON import of the document table.
fromJSON'DocTable :: (Functor m, Monad m, Applicative m, DocTable i) => Value -> m i
fromJSON'DocTable v
    = foldM ins empty $ dm'
      where
        ins res (did, doc) = update did doc res

        dm :: DocIdMap Document
        dm = case fromJSON v of
               Error _   -> DM.empty
               Success m -> m

        dm'= DM.toList . DM.map wrap $ dm

-- ------------------------------------------------------------
