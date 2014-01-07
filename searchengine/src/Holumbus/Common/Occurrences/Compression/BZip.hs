{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------
{-
  Occurrences compression using the bzip2 library
    http://www.bzip.org/

  Haskell-Bindings
    http://hackage.haskell.org/package/bzlib
-}
-- ----------------------------------------------------------------------------

module Holumbus.Common.Occurrences.Compression.BZip
  (
  -- * Compression types
    CompressedOccurrences
  , OccCompression(..)
  )
where

import           Control.DeepSeq

import           Data.Binary                             (Binary)
import qualified Data.Binary                             as B
import           Data.ByteString.Lazy                    (ByteString)

import qualified Codec.Compression.BZip                  as BZ

import qualified Holumbus.Common.DocIdMap                as DM
import           Holumbus.Common.Occurrences
import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

newtype CompressedOccurrences = ComprOccs { unComprOccs :: ByteString }
  deriving (Eq, Show, NFData)

mkComprOccs :: ByteString -> CompressedOccurrences
-- | XXX deepseq - fix if possible!
mkComprOccs b = ComprOccs $!! b

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
  compressOcc   = compress
  decompressOcc = decompress
  differenceWithKeySet ks = compress . (flip DM.diffWithSet) ks . decompress

-- ----------------------------------------------------------------------------

instance Binary CompressedOccurrences where
  put = B.put . unComprOccs
  get = B.get >>= return . mkComprOccs

-- ----------------------------------------------------------------------------

--compress :: Binary a => a -> CompressedOccurrences
compress :: Occurrences -> CompressedOccurrences
compress = mkComprOccs . BZ.compress . B.encode

--decompress :: Binary a => CompressedOccurrences -> a
decompress :: CompressedOccurrences -> Occurrences
decompress = B.decode . BZ.decompress . unComprOccs
