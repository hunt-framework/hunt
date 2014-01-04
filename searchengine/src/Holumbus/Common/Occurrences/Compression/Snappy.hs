{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , OccOSerialized(..)
  , OccCompression(..)
  )
where

import           Control.DeepSeq

import           Data.Binary                             (Binary)
import qualified Data.Binary                             as B
import           Data.ByteString.Lazy                    (ByteString)
#if  __GLASGOW_HASKELL__ >= 770
import qualified Codec.Compression.Snappy.Lazy           as BZ
#endif

import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

type CompressedOccurrences = OccOSerialized

-- ----------------------------------------------------------------------------

newtype OccOSerialized = OccOBs { unOccOBs :: ByteString }
  deriving (Eq, Show, NFData)

mkOccOBs :: ByteString -> OccOSerialized
mkOccOBs b = OccOBs $! b

-- ----------------------------------------------------------------------------
#if  __GLASGOW_HASKELL__ >= 770
instance OccCompression CompressedOccurrences where
  compressOcc           = mkOccOBs . BZ.compress . B.encode
  decompressOcc         = B.decode . BZ.decompress . unOccOBs
  differenceWithKeySet  = undefined
#else
#warning snappy is disabled if GHC < 7.7
instance OccCompression CompressedOccurrences where
  compressOcc           = mkOccOBs . B.encode
  decompressOcc         = B.decode . unOccOBs
  differenceWithKeySet  = undefined

#endif
-- ----------------------------------------------------------------------------

instance Binary OccOSerialized where
  put = B.put . unOccOBs
  get = B.get >>= return . OccOBs

-- ----------------------------------------------------------------------------
