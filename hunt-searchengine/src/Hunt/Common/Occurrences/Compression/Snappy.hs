{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- ----------------------------------------------------------------------------
{-
  Occurrences compression using Google's Snappy library
    https://code.google.com/p/snappy/

  Haskell-Bindings
    http://hackage.haskell.org/package/snappy

  Requires the Snappy C library
    source: https://code.google.com/p/snappy/
    deb: apt-get install libsnappy-dev
    rpm: yum install libsnappy-devel
-}
-- ----------------------------------------------------------------------------

module Hunt.Common.Occurrences.Compression.Snappy
  (
  -- * Compression types
    CompressedOccurrences
  , OccCompression(..)
  )
where

import           Control.Applicative                     ((<$>))
import           Control.DeepSeq

import           Data.Binary                             (Binary (..))
import qualified Data.Binary                             as B
import qualified Data.ByteString.Lazy                    as BL
import           Data.ByteString.Short                   (ShortByteString)
import qualified Data.ByteString.Short                   as Short
import           Data.Typeable

import qualified Codec.Compression.Snappy.Lazy.Smart     as ZIP

import qualified Hunt.Common.DocIdMap                    as DM
import           Hunt.Common.Occurrences
import           Hunt.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

-- ShortByteString: It has a lower memory overhead than a ByteString and and does not contribute to
-- heapfragmentation. It can be converted to or from a ByteString (at the cost of copying the string
-- data).
-- https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html#g:1

newtype CompressedOccurrences = ComprOccs { unComprOccs :: ShortByteString }
  deriving (Eq, Show, Typeable)

mkComprOccs :: ShortByteString -> CompressedOccurrences
mkComprOccs b = ComprOccs $!! b

-- ----------------------------------------------------------------------------

instance NFData CompressedOccurrences where
-- use default implementation: eval to WHNF, and that's sufficient

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
  compressOcc           = compress
  decompressOcc         = decompress
  differenceWithKeySet ks = compress . (flip DM.diffWithSet) ks . decompress

-- ----------------------------------------------------------------------------

instance Binary CompressedOccurrences where
  put = put . Short.fromShort . unComprOccs
  get = mkComprOccs . Short.toShort <$> get

-- to avoid sharing the data with the input the ByteString is physically copied
-- before return. This should be the single place where sharing is introduced,
-- else the copy must be moved to mSBs


-- ----------------------------------------------------------------------------

--compress :: Binary a => a -> CompressedOccurrences
compress :: Occurrences -> CompressedOccurrences
compress = mkComprOccs . Short.toShort . BL.toStrict . ZIP.compress . B.encode

--decompress :: Binary a => CompressedOccurrences -> a
decompress :: CompressedOccurrences -> Occurrences
decompress = B.decode . ZIP.decompress . BL.fromStrict . Short.fromShort . unComprOccs
