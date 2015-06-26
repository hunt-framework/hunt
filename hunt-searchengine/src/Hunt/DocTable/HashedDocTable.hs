{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.HashedDocTable
  Copyright  : Copyright (C) 2012 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
  Stability  : experimental

  A more space efficient substitute for Hunt.Index.Documents
  and a more flexible implementation than Hunt.Index.CompactDocuments.

  DocIds are computed by a hash function, so the inverse map from 'URI's to 'DocId's
  is substituted by the hash function.

  MurmurHash2 64-bit is used as the hash function.
  https://sites.google.com/site/murmurhash/

  It is a fast non-cryptographic hash function with good performance and
  hash distribution properties.
  http://programmers.stackexchange.com/a/145633
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

import           Control.DeepSeq

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId      (DocId, mkDocId)
import           Hunt.Common.DocIdMap   (DocIdMap)
import qualified Hunt.Common.DocIdMap   as DM
import           Hunt.Common.DocIdSet   (DocIdSet)
import           Hunt.Common.Document   (Document (..), DocumentWrapper (..))
import           Hunt.DocTable
import           Hunt.Utility

-- ------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.
type DocMap e
  = DocIdMap e

-- | The 'DocTable' implementation. Maps 'DocId's to 'Document's.
newtype Documents e
  = Documents { idToDoc :: DocMap e }     -- ^ A mapping from a document id to
                                          --   the document itself.
  deriving (Eq, Show)

mkDocuments :: NFData e => DocMap e -> Documents e
mkDocuments m = Documents $! m

-- ------------------------------------------------------------

instance NFData e => NFData (Documents e) where
  rnf (Documents e) = rnf e

instance (DocumentWrapper e, Binary e) => Binary (Documents e) where
  put = put . idToDoc
  get = get >>= return . mkDocuments

--- ------------------------------------------------------------

-- | An empty document table.
empty' :: (DocTable (Documents e), DocumentWrapper e) => Documents e
empty' = mkDocuments DM.empty

-- | Build a 'DocTable' from a 'DocIdMap' (maps 'DocId's to 'Document's)
fromMap :: (DocTable (Documents e), DocumentWrapper e) =>
          (Document -> e) -> DocIdMap Document -> Documents e
fromMap = fromMap'

-- ------------------------------------------------------------

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

  restrict    = return .:: restrict'

  -- Convert document table to a single map.
  toMap       = return . toMap'

  -- | Empty 'DocTable'.
  empty       = empty'

-- ------------------------------------------------------------

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
  i = mkDocId u

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
    = mkDocuments $ idToDoc dt1' `DM.union` idToDoc dt2'


insert'     :: (DocumentWrapper e) => e -> Documents e -> (DocId, Documents e)
insert' d ds
  = maybe reallyInsert (const (newId, ds)) (lookup' newId ds)
  where
  newId
      = mkDocId . uri . unwrap $ d
  reallyInsert
      = (newId, mkDocuments $ DM.insert newId d $ idToDoc ds)

update'     :: (DocumentWrapper e) => DocId -> e -> Documents e -> Documents e
update' i d ds
  = mkDocuments $ DM.insert i d $ idToDoc ds

delete'     :: (DocumentWrapper e) => DocId -> Documents e -> Documents e
delete' d ds
  = mkDocuments $ DM.delete d $ idToDoc ds

difference' :: (DocumentWrapper e) => DocIdSet -> Documents e -> Documents e
difference' s ds
  = mkDocuments $ idToDoc ds `DM.diffWithSet` s

map'        :: (DocumentWrapper e) => (e -> e) -> Documents e -> Documents e
map' f d
  = mkDocuments $ DM.map f (idToDoc d)

filter'     :: (DocumentWrapper e) => (e -> Bool) -> Documents e -> Documents e
filter' p d
  = mkDocuments $ DM.filter p (idToDoc d)

restrict'   :: (DocumentWrapper e) => DocIdSet -> Documents e -> Documents e
restrict' dIds d
  = mkDocuments $ DM.intersectionWithSet (idToDoc d) dIds

fromMap'    :: (DocumentWrapper e) => (Document -> e) -> DocIdMap Document -> Documents e
fromMap' f itd
  = mkDocuments $ DM.map f itd

toMap'      :: Documents e -> DocIdMap e
toMap'
  = idToDoc

-- ------------------------------------------------------------
