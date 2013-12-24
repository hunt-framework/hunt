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
import qualified Data.ByteString.Lazy                    as BL

import qualified Codec.Compression.Snappy.Lazy           as BZ

import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

type CompressedOccurrences = OccOSerialized

-- ----------------------------------------------------------------------------

newtype OccOSerialized = OccOBs { unOccOBs :: ByteString }
  deriving (Eq, Show, NFData)

mkOccOBs :: ByteString -> OccOSerialized
mkOccOBs b = OccOBs $! b

-- ----------------------------------------------------------------------------

newtype ByteString = Bs { unBs :: BL.ByteString }
  deriving (Eq, Show)

mkBs :: BL.ByteString -> ByteString
mkBs s = Bs $!! s

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
  compressOcc           = mkOccOBs . mkBs . BZ.compress . B.encode
  decompressOcc         = B.decode . BZ.decompress . unBs . unOccOBs
  differenceWithKeySet  = undefined

-- ----------------------------------------------------------------------------

instance Binary OccOSerialized where
  put = B.put . unOccOBs
  get = B.get >>= return . OccOBs

-- ----------------------------------------------------------------------------

instance NFData ByteString where
-- use default implementation: eval to WHNF, and that's sufficient

-- ----------------------------------------------------------------------------

instance Binary ByteString where
   put = B.put . unBs
   get = B.get >>= return . mkBs

-- ----------------------------------------------------------------------------
