{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.HashedDocuments
  Copyright  : Copyright (C) 2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental

  A more space efficient substitute for Holumbus.Index.Documents
  and a more flexible implementation than Holumbus.Index.CompactDocuments.

  DocIds are computed by a hash function, so the inverse map from URIs to DocIds
  is substituted by the hash function
-}

-- ----------------------------------------------------------------------------

module Holumbus.DocTable.HashedDocuments
    (
      -- * Documents type
      Documents (..)
    , DocMap

      -- * Construction
    , empty

      -- * Conversion
    , fromMap
    )
where

import           Data.Binary                    (Binary (..))
import qualified Data.Binary                    as B

import           Data.Digest.Murmur64

import           Holumbus.DocTable.Common
import qualified Holumbus.Common.DocIdMap as DM

import           Holumbus.DocTable.DocTable

-- ----------------------------------------------------------------------------

-- XXX: add DocumentWrapper constraint?
-- | The table which is used to map a document to an artificial id and vice versa.
type DocMap e
    = DocIdMap e

-- | The 'DocTable' implementation. Maps 'DocId's to 'Document's.
newtype Documents e
    = Documents { idToDoc :: DocMap e }     -- ^ A mapping from a document id to
                                            --   the document itself.
      deriving (Eq, Show)

-- ----------------------------------------------------------------------------

instance (DocumentWrapper e, Binary e) => Binary (Documents e) where
  put = put . idToDoc
  get = get >>= return . Documents

--- ----------------------------------------------------------------------------

-- | An empty document table.
empty :: (DocTable (Documents e), DocumentWrapper e) => Documents e
empty = Documents DM.empty

-- | The hash function from URIs to DocIds
docToId :: URI -> DocId
docToId = mkDocId . fromIntegral . asWord64 . hash64 . B.encode

-- | Build a 'DocTable' from a 'DocIdMap' (maps 'DocId's to 'Document's)
fromMap :: (DocTable (Documents e), DocumentWrapper e) =>
          (Document -> e) -> DocIdMap Document -> Documents e
fromMap = fromMap'

-- ----------------------------------------------------------------------------

instance (DocumentWrapper e) =>
         DocTable (Documents e) where
    type DValue (Documents e) = e
    null        = null'

    -- Returns the number of unique documents in the table.
    size        = size'

    -- Lookup a document by its id.
    lookup      = lookup'

    -- Lookup the id of a document by an URI.
    lookupByURI = lookupByURI'

    -- Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds
    union       = unionDocs'

    -- Test whether the doc ids of both tables are disjoint.
    disjoint    = disjoint'

    -- Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.
    insert      = insert'

    -- Update a document with a certain DocId.
    update      = update'

    -- Removes the document with the specified id from the table.
    delete      = delete'

    -- Deletes a set of Docs by Id from the table.
    difference  = difference'

    -- Update documents (through mapping over all documents).
    map         = map'

    -- Filters all documents that satisfy the predicate.
    filter      = filter'

    -- Convert document table to a single map
    toMap       = toMap'

    -- Edit document ids
    mapKeys     = error "hashed doctables cannot change ids"

-- ----------------------------------------------------------------------------

null'       :: (DocumentWrapper e) => Documents e -> Bool
null'
    = DM.null . idToDoc

size'       :: (DocumentWrapper e) => Documents e -> Int
size'
    = DM.size . idToDoc

lookup'     :: (Monad m, DocumentWrapper e) => Documents e -> DocId -> m e
lookup'  d i
    = maybe (fail "") return
      . DM.lookup i
      . idToDoc
      $ d

lookupByURI' :: (Monad m, DocumentWrapper e) => Documents e -> URI -> m DocId
lookupByURI' d u
    = maybe (fail "") (const $ return i)
      . DM.lookup i
      . idToDoc
      $ d
      where
        i = docToId u

disjoint'   :: (DocumentWrapper e) => Documents e -> Documents e -> Bool
disjoint' dt1 dt2
    = DM.null $ DM.intersection (idToDoc dt1) (idToDoc dt2)

unionDocs'  :: (DocumentWrapper e) => Documents e -> Documents e -> Documents e
unionDocs' dt1 dt2
    | disjoint' dt1 dt2
        = unionDocs'' dt1 dt2
    | otherwise
        = error
          "HashedDocuments.unionDocs: doctables are not disjoint"
    where
    unionDocs'' :: (DocumentWrapper e) => Documents e -> Documents e -> Documents e
    unionDocs'' dt1' dt2'
        = Documents
          { idToDoc = idToDoc dt1' `DM.union` idToDoc dt2' }


insert'     :: (DocumentWrapper e) => Documents e -> e -> (DocId, Documents e)
insert' ds d
    = maybe reallyInsert (const (newId, ds)) (lookup' ds newId)
      where
        newId
            = docToId . uri . unwrap $ d
        reallyInsert
            = (newId, Documents {idToDoc = DM.insert newId d $ idToDoc ds})

update'     :: (DocumentWrapper e) => Documents e -> DocId -> e -> Documents e
update' ds i d
    = Documents {idToDoc = DM.insert i d $ idToDoc ds}

delete'     :: (DocumentWrapper e) => Documents e -> DocId -> Documents e
delete' ds d
    = Documents {idToDoc = DM.delete d $ idToDoc ds}

difference' :: (DocumentWrapper e) => DM.DocIdSet -> Documents e -> Documents e
difference' s ds
    = Documents {idToDoc = idToDoc ds `DM.diffWithSet` s}

map'        :: (DocumentWrapper e) => (e -> e) -> Documents e -> Documents e
map' f d
    = Documents {idToDoc = DM.map f (idToDoc d)}

filter'     :: (DocumentWrapper e) => (e -> Bool) -> Documents e -> Documents e
filter' p d
    = Documents {idToDoc = DM.filter p (idToDoc d)}

fromMap'    :: (DocumentWrapper e) => (Document -> e) -> DocIdMap Document -> Documents e
fromMap' f itd
    = Documents {idToDoc = DM.map f itd}

toMap'      :: (DocumentWrapper e) => Documents e -> DocIdMap e
toMap'
    = idToDoc

-- ------------------------------------------------------------
