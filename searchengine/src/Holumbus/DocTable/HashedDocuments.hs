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

import           Control.Arrow                  (second)

import qualified Data.Binary                    as B
import           Data.Set                       (Set)
import qualified Data.Set                       as S

import           Data.Digest.Murmur64

import           Holumbus.Index.Common          hiding (empty)
import qualified Holumbus.Index.Common.DocIdMap as DM

import           Holumbus.DocTable.DocTable     hiding (map)

import           Holumbus.Utility               ((.::))

-- ----------------------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.
type DocMap
    = DocIdMap DocumentWrapper
{-
-- | The Document as a bzip-compressed bytestring.
newtype CompressedDoc
    = CDoc { unCDoc :: ByteString }
      deriving (Eq, Show)
-}
-- | The 'DocTable' implementation. Maps 'DocId's to 'Document's.
newtype Documents
    = Documents { idToDoc   :: DocMap }     -- ^ A mapping from a document id to
                                            --   the document itself.
      --deriving (Eq, Show, NFData)

-- ----------------------------------------------------------------------------

-- | An empty document table.
empty :: DocTable Documents DocumentWrapper
empty = newDocTable emptyDocuments

-- | The hash function from URIs to DocIds
docToId :: URI -> DocId
docToId = mkDocId . fromIntegral . asWord64 . hash64 . B.encode

-- | Build a 'DocTable' from a 'DocIdMap' (maps 'DocId's to 'Document's)
fromMap :: (DocumentRaw -> DocumentWrapper) -> DocIdMap DocumentRaw -> DocTable Documents DocumentWrapper
fromMap = newDocTable .:: fromMap'

-- ----------------------------------------------------------------------------

newDocTable :: Documents -> DocTable Documents DocumentWrapper
newDocTable i =
    Dt
    {
    -- Test whether the doc table is empty.
      _null                          = null' i

    -- Returns the number of unique documents in the table.
    , _size                          = size' i

    -- Lookup a document by its id.
    , _lookup                        = lookupById' i

    -- Lookup the id of a document by an URI.
    , _lookupByURI                   = lookupByURI' i

    -- Union of two disjoint document tables. It is assumed, that the DocIds and the document uris
    -- of both indexes are disjoint. If only the sets of uris are disjoint, the DocIds can be made
    -- disjoint by adding maxDocId of one to the DocIds of the second, e.g. with editDocIds
    , _union                         = newDocTable . unionDocs' i . impl

    -- Test whether the doc ids of both tables are disjoint.
    , _disjoint                      = disjointDocs' i . impl

    -- Insert a document into the table. Returns a tuple of the id for that document and the
    -- new table. If a document with the same URI is already present, its id will be returned
    -- and the table is returned unchanged.
    , _insert                        = second newDocTable . insertDoc' i

    -- Update a document with a certain DocId.
    , _update                        = newDocTable .:: updateDoc' i

    -- Removes the document with the specified id from the table.
    , _delete                        = newDocTable . deleteById' i

    -- Deletes a set of Docs by Id from the table.
    , _difference                    = \ids -> newDocTable $ differenceById' ids i

    -- Update documents (through mapping over all documents).
    , _map                           = \f -> newDocTable $ updateDocuments' f i

    -- Filters all documents that satisfy the predicate.
    , _filter                        = \f -> newDocTable $ filterDocuments' f i

    -- Convert document table to a single map
    , _toMap                         = toMap' i

    -- Edit document ids
    , _mapKeys                       = error "hashed doctables cannot change ids"

    -- The doctable implementation.
    , _impl                          = i
    }

-- ----------------------------------------------------------------------------

null' :: Documents -> Bool
null'
    = DM.null . idToDoc

size' :: Documents -> Int
size'
    = DM.size . idToDoc

lookupById' :: Monad m => Documents -> DocId -> m DocumentWrapper
lookupById'  d i
    = maybe (fail "") return
      . DM.lookup i
      . idToDoc
      $ d

lookupByURI' :: Monad m => Documents -> URI -> m DocId
lookupByURI' d u
    = maybe (fail "") (const $ return i)
      . DM.lookup i
      . idToDoc
      $ d
      where
        i = docToId u

disjointDocs' :: Documents -> Documents -> Bool
disjointDocs' dt1 dt2
    = DM.null $ DM.intersection (idToDoc dt1) (idToDoc dt2)

unionDocs' :: Documents -> Documents -> Documents
unionDocs' dt1 dt2
    | disjointDocs' dt1 dt2
        = unionDocsX dt1 dt2
    | otherwise
        = error
          "HashedDocuments.unionDocs: doctables are not disjoint"

insertDoc' :: Documents -> DocumentWrapper -> (DocId, Documents)
insertDoc' ds d
    = maybe reallyInsert (const (newId, ds)) (lookupById' ds newId)
      where
        d'  = doc d
        newId
            = docToId . uri $ d'
        reallyInsert
            = (newId, Documents {idToDoc = DM.insert newId d $ idToDoc ds})

updateDoc' :: Documents -> DocId -> DocumentWrapper -> Documents
updateDoc' ds i d
    = Documents {idToDoc = DM.insert i d $ idToDoc ds}

deleteById' :: Documents -> DocId -> Documents
deleteById' ds d
    = Documents {idToDoc = DM.delete d $ idToDoc ds}

-- XXX: EnumMap does not have a fromSet function so that you can use fromSet (const ()) and ignore the value
differenceById' :: Set DocId -> Documents -> Documents
differenceById' s ds
    = Documents {idToDoc = idToDoc ds `DM.difference` (DM.fromAscList . map mkKeyValueDummy . S.toList $ s)}
    where
    mkKeyValueDummy k = (k, undefined) -- XXX: strictness properties of EnumMap?

updateDocuments' :: (DocumentWrapper -> DocumentWrapper) -> Documents -> Documents
updateDocuments' f d
    = Documents {idToDoc = DM.map f (idToDoc d)}

filterDocuments' :: (DocumentWrapper -> Bool) -> Documents -> Documents
filterDocuments' p d
    = Documents {idToDoc = DM.filter p (idToDoc d)}

fromMap' :: (DocumentRaw -> DocumentWrapper) -> DocIdMap DocumentRaw -> Documents
fromMap' f itd
    = Documents {idToDoc = DM.map f itd}

toMap' :: Documents -> DocIdMap DocumentWrapper
toMap'
    = idToDoc

-- ------------------------------------------------------------

-- | Create an empty table.
emptyDocuments :: Documents
emptyDocuments
    = Documents DM.empty


unionDocsX :: Documents -> Documents -> Documents
unionDocsX dt1 dt2
    = Documents
      { idToDoc = idToDoc dt1 `DM.union` idToDoc dt2 }
