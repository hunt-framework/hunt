{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- ----------------------------------------------------------------------------
{-
  Occurrences compression using the bzip2 library
    http://www.bzip.org/

  Note: This implementation leads to huge heap fragmentation
  due to pinned memory. This module exists for benchmarking
  reasons, don't use it in production. Use module BZip instead.
  It uses ShortByteString resulting in no fragmentation and less
  memory usage.

  Haskell-Bindings
    http://hackage.haskell.org/package/bzlib
-}
-- ----------------------------------------------------------------------------

module Hunt.Common.Occurrences.Compression.BZipBs
  (
  -- * Compression types
    CompressedOccurrences
  , OccCompression(..)
  )
where

import qualified Codec.Compression.BZip.Smart            as ZIP

import           Control.Applicative                     ((<$>))
import           Control.DeepSeq

import           Data.Binary                             (Binary (..))
import qualified Data.Binary                             as B
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Typeable

import qualified Hunt.Common.DocIdMap                    as DM
import           Hunt.Common.Occurrences
import           Hunt.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

-- ShortByteString: It has a lower memory overhead than a ByteString and and does not contribute to
-- heapfragmentation. It can be converted to or from a ByteString (at the cost of copying the string
-- data).
-- https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html#g:1

newtype CompressedOccurrences = ComprOccs { unComprOccs :: BS.ByteString }
  deriving (Eq, Show, Typeable)

mkComprOccs :: BS.ByteString -> CompressedOccurrences
mkComprOccs b = ComprOccs $!! b

-- ----------------------------------------------------------------------------

instance NFData CompressedOccurrences where
-- use default implementation: eval to WHNF, and that's sufficient

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
  compressOcc   = compress
  decompressOcc = decompress
  differenceWithKeySet ks = compress . (flip DM.diffWithSet) ks . decompress

-- ----------------------------------------------------------------------------

instance Binary CompressedOccurrences where
  put = put . unComprOccs
  get = mkComprOccs . BS.copy <$> get

-- to avoid sharing the data with the input the ByteString is physically copied
-- before return. This should be the single place where sharing is introduced,
-- else the copy must be moved to mSBs

-- ----------------------------------------------------------------------------

--compress :: Binary a => a -> CompressedOccurrences
compress :: Occurrences -> CompressedOccurrences
compress = mkComprOccs . BL.toStrict . ZIP.compress . B.encode

--decompress :: Binary a => CompressedOccurrences -> a
decompress :: CompressedOccurrences -> Occurrences
decompress = B.decode . ZIP.decompress . BL.fromStrict . unComprOccs

-- ----------------------------------------------------------------------------
