{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Holumbus.Index.HashedDocuments
    (
      -- * Documents type
      Documents (..)
    , CompressedDoc(..)
    , DocMap

      -- * Construction
    , emptyDocuments
    , singleton

      -- * Conversion
    , toDocument
    , fromDocument
    , fromDocMap
    , toDocMap
    )
where

import qualified Codec.Compression.BZip as BZ

import           Control.DeepSeq

import qualified Data.Set               as S
import           Data.Binary            ( Binary )
import qualified Data.Binary            as B

import           Data.ByteString.Lazy   ( ByteString )
import qualified Data.ByteString.Lazy   as BS

import           Data.Digest.Murmur64

import           Holumbus.Index.Common

-- ----------------------------------------------------------------------------

-- | The table which is used to map a document to an artificial id and vice versa.

type DocMap
    = DocIdMap CompressedDoc

newtype CompressedDoc
    = CDoc { unCDoc :: ByteString }
      deriving (Eq, Show)

newtype Documents
    = Documents { idToDoc   :: DocMap }     -- ^ A mapping from a document id to
                                            --   the document itself.
      deriving (Eq, Show, NFData)

-- ----------------------------------------------------------------------------
-- | The hash function from URIs to DocIds

docToId :: URI -> DocId
docToId = mkDocId . fromIntegral . asWord64 . hash64 . B.encode

-- ----------------------------------------------------------------------------

instance HolIndex i => HolDocIndex Documents i where
    unionDocIndex dt1 ix1 dt2 ix2
        | s1 == 0
            = (dt2, ix2)
        | s2 == 0
            = (dt1, ix1)
        | s1 < s2
            = unionDocIndex dt2 ix2 dt1 ix1
        | otherwise
            = (dt, ix)
        where
          dt = unionDocs'    dt1  dt2   -- the unchecked version of union of doctables
          ix = mergeIndexes  ix1  ix2
          s1 = sizeDocs dt1
          s2 = sizeDocs dt2

-- ----------------------------------------------------------------------------

toDocument                      :: CompressedDoc -> Document
toDocument                      = B.decode . BZ.decompress . unCDoc

fromDocument                    :: Document -> CompressedDoc
fromDocument                    = CDoc . BZ.compress . B.encode

mapDocument                     :: (Document -> Document) -> CompressedDoc -> CompressedDoc
mapDocument f                   = fromDocument . f . toDocument

toDocMap                        :: DocIdMap Document -> DocMap
toDocMap                        = mapDocIdMap fromDocument

fromDocMap                      :: DocMap -> DocIdMap Document
fromDocMap                      = mapDocIdMap toDocument

-- ----------------------------------------------------------------------------

instance HolDocuments Documents where
  nullDocs
      = nullDocIdMap . idToDoc

  sizeDocs
      = sizeDocIdMap . idToDoc

  lookupById  d i
      = maybe (fail "") return
        . fmap toDocument
        . lookupDocIdMap i
        . idToDoc
        $ d

  lookupByURI d u
      = maybe (fail "") (const $ return i)
        . lookupDocIdMap i
        . idToDoc
        $ d
        where
          i = docToId u

  disjointDocs dt1 dt2
      = nullDocIdMap $ intersectionDocIdMap (idToDoc dt1) (idToDoc dt2)

  unionDocs dt1 dt2
      | disjointDocs dt1 dt2
          = unionDocs' dt1 dt2
      | otherwise
          = error $
            "HashedDocuments.unionDocs: doctables are not disjoint"

  makeEmpty _
      = emptyDocuments

  insertDoc ds d
      = maybe reallyInsert (const (newId, ds)) (lookupById ds newId)
        where
          newId
              = docToId . uri $ d
          d'  = fromDocument d
          reallyInsert
              = rnf d' `seq`                    -- force document compression
                (newId, Documents {idToDoc = insertDocIdMap newId d' $ idToDoc ds})

  updateDoc ds i d
      = rnf d' `seq`                    -- force document compression
        Documents {idToDoc = insertDocIdMap i d' $ idToDoc ds}
      where
        d'                      = fromDocument d

  removeById ds d
      = Documents {idToDoc = deleteDocIdMap d $ idToDoc ds}


  -- XXX: EnumMap does not have a fromSet function so that you can use fromSet (const ()) and ignore the value
  deleteById s ds
      = Documents {idToDoc = idToDoc ds `differenceDocIdMap` (fromAscListDocIdMap . map mkKeyValueDummy . S.toList $ s)}
      where
      mkKeyValueDummy k = (k, undefined) -- XXX: strictness properties of EnumMap?


  updateDocuments f d
      = Documents {idToDoc = mapDocIdMap (mapDocument f) (idToDoc d)}


  filterDocuments p d
      = Documents {idToDoc = filterDocIdMap (p . toDocument) (idToDoc d)}


  fromMap itd'
      = Documents {idToDoc = toDocMap itd'}

  toMap
      = fromDocMap . idToDoc

-- ----------------------------------------------------------------------------

instance Binary Documents where
    put = B.put . idToDoc
    get = fmap Documents B.get

-- ------------------------------------------------------------

instance Binary CompressedDoc where
    put = B.put . unCDoc
    get = B.get >>= return . CDoc

-- ----------------------------------------------------------------------------

instance NFData CompressedDoc where
    rnf (CDoc s)
        = BS.length s `seq` ()

-- ------------------------------------------------------------

-- | Create an empty table.

emptyDocuments :: Documents
emptyDocuments
    = Documents emptyDocIdMap

unionDocs' :: Documents -> Documents -> Documents
unionDocs' dt1 dt2
    = Documents
      { idToDoc = unionDocIdMap (idToDoc dt1) (idToDoc dt2) }

-- | Create a document table containing a single document.

singleton :: Document -> Documents
singleton d
    = rnf d' `seq`
      Documents {idToDoc = singletonDocIdMap (docToId . uri $ d) d'}
    where
      d' = fromDocument d

-- ------------------------------------------------------------
