{-# LANGUAGE CPP                        #-}
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

module Holumbus.Common.Occurrences.Compression.Snappy
  (
  -- * Compression types
    CompressedOccurrences
  , OccCompression(..)
  )
where

import           Control.DeepSeq

import           Data.Binary                             (Binary (..))
import qualified Data.Binary                             as B
import           Data.ByteString.Lazy                    (ByteString)
#if  __GLASGOW_HASKELL__ >= 770
import qualified Codec.Compression.Snappy.Lazy           as BZ
#endif

import qualified Holumbus.Common.DocIdMap                as DM
import           Holumbus.Common.Occurrences
import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

newtype CompressedOccurrences = ComprOccs { unComprOccs :: ByteString }
  deriving (Eq, Show, NFData)

mkComprOccs :: ByteString -> CompressedOccurrences
mkComprOccs b = ComprOccs $!! b

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
  compressOcc           = compress
  decompressOcc         = decompress
  differenceWithKeySet ks = compress . (flip DM.diffWithSet) ks . decompress

-- ----------------------------------------------------------------------------

instance Binary CompressedOccurrences where
  put = put . unComprOccs
  get = get >>= return . mkComprOccs

-- ----------------------------------------------------------------------------

#if  __GLASGOW_HASKELL__ >= 770
--compress :: Binary a => a -> CompressedOccurrences
compress :: Occurrences -> CompressedOccurrences
compress = mkComprOccs . BZ.compress . B.encode

--decompress :: Binary a => CompressedOccurrences -> a
decompress :: CompressedOccurrences -> Occurrences
decompress = B.decode . BZ.decompress . unComprOccs

#else
#warning snappy is disabled if GHC < 7.7
--compress :: Binary a => a -> CompressedOccurrences
compress :: Occurrences -> CompressedOccurrences
compress = mkComprOccs . B.encode

--decompress :: Binary a => CompressedOccurrences -> a
decompress :: CompressedOccurrences -> Occurrences
decompress = B.decode . unComprOccs
#endif
