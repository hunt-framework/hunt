{-# LANGUAGE ConstraintKinds   #-}
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

import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Maybe             (catMaybes)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           GHC.Exts
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap   (DocIdMap (..))
import qualified Hunt.Common.DocIdMap   as DM
import           Hunt.Common.DocIdSet   (DocIdSet)
import qualified Hunt.Common.DocIdSet   as DS
import           Hunt.Common.Document   (Document,
                                         DocumentWrapper (wrap, unwrap))
import           Prelude                hiding (Word, filter, lookup, map, null)


-- ------------------------------------------------------------

-- | The document table type class which needs to be implemented to be used by the 'Interpreter'.
--   The type parameter @i@ is the implementation.
--   The implementation must have a value type parameter.

class (DocumentWrapper (DValue i), NFData i) => DocTable i where
    -- | The value type of the document table.
    type DValue i :: *

    type Cxt (m :: * -> *) i :: Constraint

    -- | Test whether the document table is empty.
    null            :: (Cxt m i, Monad m) => i -> m Bool

    -- | Returns the number of unique documents in the table.
    size            :: (Cxt m i, Monad m) => i -> m Int

    -- | Lookup a document by its ID.
    lookup          :: (Cxt m i, Monad m) => DocId -> i -> m (Maybe (DValue i))

    -- | Lookup the 'DocId' of a document by an 'URI'.
    lookupByURI     :: (Cxt m i, Monad m) => URI -> i -> m (Maybe DocId)

    -- | Union of two disjoint document tables. It is assumed, that the
    --   DocIds and the document 'URI's of both indexes are disjoint.
    union           :: (Cxt m i, Monad m) => i -> i -> m i

    -- | Test whether the 'DocId's of both tables are disjoint.
    disjoint        :: (Cxt m i, Monad m) => i -> i -> m Bool

    -- | Insert a document into the table. Returns a tuple of the 'DocId' for that document and the
    --   new table. If a document with the same 'URI' is already present, its id will be returned
    --   and the table is returned unchanged.
    insert          :: (Cxt m i, Monad m) => DValue i -> i -> m (DocId, i)

    -- | Update a document with a certain 'DocId'.
    update          :: (Cxt m i, Monad m) => DocId -> DValue i -> i -> m i

    -- | Update a document by 'DocId' with the result of the provided function.
    adjust          :: (Cxt m i, Monad m) => (DValue i -> m (DValue i)) -> DocId -> i -> m i
    adjust f did d =
        maybe (return d) (upd d did <=< f) =<< lookup did d
        --maybe d (update d did . f) $ lookup d did
        where upd i docid v = update docid v i

    -- | Update a document by 'URI' with the result of the provided function.
    adjustByURI     :: (Cxt m i, Monad m) => (DValue i -> m (DValue i)) -> URI -> i -> m i
    adjustByURI f uri d
        = maybe (return d) (flip (adjust f) d) =<< lookupByURI uri d

    -- | Removes the document with the specified 'DocId' from the table.
    delete          :: (Cxt m i, Monad m) => DocId -> i -> m i

    -- | Removes the document with the specified 'URI' from the table.
    deleteByURI     :: (Cxt m i, Monad m) => URI -> i -> m i
    deleteByURI u ds
        = maybe (return ds) (flip delete ds) =<< lookupByURI u ds

    -- | Deletes a set of documents by 'DocId' from the table.
    difference      :: (Cxt m i, Monad m) => DocIdSet -> i -> m i

    -- | Deletes a set of documents by 'URI' from the table.
    differenceByURI :: (Cxt m i, Monad m) => Set URI -> i -> m i
    differenceByURI uris d = do -- XXX: eliminate S.toList?
        ids <- liftM (DS.fromList . catMaybes) . mapM (flip lookupByURI d) . S.toList $ uris
        difference ids d

    -- | Map a function over all values of the document table.
    map             :: (Cxt m i, Monad m) => (DValue i -> DValue i) -> i -> m i

    -- | Filters all documents that satisfy the predicate.
    filter          :: (Cxt m i, Monad m) => (DValue i -> Bool) -> i -> m i

    restrict        :: (Cxt m i, Monad m) => DocIdSet -> i -> m i

    -- | Convert document table to a 'DocIdMap'.
    toMap           :: (Cxt m i, Monad m) => i -> m (DocIdMap (DValue i))

    docIds          :: (Cxt m i, Monad m) => i -> m DocIdSet

    -- | Empty 'DocTable'.
    empty           :: i


-- restrict :: (Functor m, Monad m, Applicative m, DocTable i) => DocIdSet -> i -> m i
-- restrict is dt
--    = foldM ins empty $ DS.toList is
--      where
--        ins m i = do v <- fromJust <$> lookup i dt
--                     update i v m

-- ------------------------------------------------------------

-- | JSON dump of the document table.
toJSON'DocTable :: (Functor m, Monad m, Applicative m, DocTable i, Cxt m i) => i -> m Value
toJSON'DocTable dt
    = do didm <- DM.map unwrap <$> toMap dt
         return $ toJSON didm

-- | JSON import of the document table.
fromJSON'DocTable :: (Functor m, Monad m, Applicative m, DocTable i, Cxt m i) => Value -> m i
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
