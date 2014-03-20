{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.HashedDocTable
  Copyright  : Copyright (C) 2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental

  A more space efficient substitute for Hunt.Index.Documents
  and a more flexible implementation than Hunt.Index.CompactDocuments.

  DocIds are computed by a hash function, so the inverse map from URIs to DocIds
  is substituted by the hash function
-}

-- ----------------------------------------------------------------------------

module Hunt.DocTable.HashedDocTable
    (
      -- * Documents type
      Documents (..)
    , DocMap

      -- * Conversion
    , fromMap
    )
where

import           Data.Binary            (Binary (..))
import qualified Data.Binary            as B

import           Data.Digest.Murmur64

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId      (DocId)
import qualified Hunt.Common.DocId      as DId
import           Hunt.Common.DocIdMap   (DocIdMap)
import qualified Hunt.Common.DocIdMap   as DM
import           Hunt.Common.Document   (Document (..), DocumentWrapper (..))
import           Hunt.DocTable

import           Hunt.Utility

-- ----------------------------------------------------------------------------

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
empty' :: (DocTable (Documents e), DocumentWrapper e) => Documents e
empty' = Documents DM.empty

-- | The hash function from URIs to DocIds
docToId :: URI -> DocId
docToId = DId.fromInteger . fromIntegral . asWord64 . hash64 . B.encode

-- | Build a 'DocTable' from a 'DocIdMap' (maps 'DocId's to 'Document's)
fromMap :: (DocTable (Documents e), DocumentWrapper e) =>
          (Document -> e) -> DocIdMap Document -> Documents e
fromMap = fromMap'

-- ----------------------------------------------------------------------------

instance (DocumentWrapper e) =>
         DocTable (Documents e) where
  type DValue (Documents e) = e
  null        = return . null'

  -- Returns the number of unique documents in the table.
  size        = return . size'

  -- Lookup a document by its id.
  lookup      = return .:: lookup'

  -- Lookup the id of a document by an URI.
  lookupByURI = return .:: lookupByURI'

  -- Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
  -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
  -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds
  union       = return .:: unionDocs'

  -- Test whether the doc ids of both tables are disjoint.
  disjoint    = return .:: disjoint'

  -- Insert a document into the table. Returns a tuple of the id for that document and the
  -- new table. If a document with the same URI is already present, its id will be returned
  -- and the table is returned unchanged.
  insert      = return .:: insert'

  -- Update a document with a certain DocId.
  update      = return .::: update'

  -- Removes the document with the specified id from the table.
  delete      = return .:: delete'

  -- Deletes a set of Docs by Id from the table.
  difference  = return .:: difference'

  -- Update documents (through mapping over all documents).
  map         = return .:: map'

  -- Filters all documents that satisfy the predicate.
  filter      = return .:: filter'

  -- Convert document table to a single map.
  toMap       = return . toMap'

  -- Edit document ids.
  mapKeys     = error "DocTable.mapKeys: HashedDocTable"

  -- | Empty 'DocTable'.
  empty       = empty'

-- ----------------------------------------------------------------------------

null'       :: (DocumentWrapper e) => Documents e -> Bool
null'
  = DM.null . idToDoc

size'       :: (DocumentWrapper e) => Documents e -> Int
size'
  = DM.size . idToDoc

lookup'     :: (Monad m, DocumentWrapper e) => DocId -> Documents e -> m e
lookup' i d
  = maybe (fail "") return
    . DM.lookup i
    . idToDoc
    $ d

lookupByURI' :: (Monad m, DocumentWrapper e) => URI -> Documents e -> m DocId
lookupByURI' u d
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
        "HashedDocTable.unionDocs: doctables are not disjoint"
  where
  unionDocs'' :: (DocumentWrapper e) => Documents e -> Documents e -> Documents e
  unionDocs'' dt1' dt2'
    = Documents
      { idToDoc = idToDoc dt1' `DM.union` idToDoc dt2' }


insert'     :: (DocumentWrapper e) => e -> Documents e -> (DocId, Documents e)
insert' d ds
  = maybe reallyInsert (const (newId, ds)) (lookup' newId ds)
  where
  newId
      = docToId . uri . unwrap $ d
  reallyInsert
      = (newId, Documents {idToDoc = DM.insert newId d $ idToDoc ds})

update'     :: (DocumentWrapper e) => DocId -> e -> Documents e -> Documents e
update' i d ds
  = Documents {idToDoc = DM.insert i d $ idToDoc ds}

delete'     :: (DocumentWrapper e) => DocId -> Documents e -> Documents e
delete' d ds
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

toMap'      :: Documents e -> DocIdMap e
toMap'
  = idToDoc

-- ------------------------------------------------------------
