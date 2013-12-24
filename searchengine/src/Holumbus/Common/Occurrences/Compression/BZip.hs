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
  , OccOSerialized(..)
  , OccCompression(..)
  )
where

import           Control.DeepSeq

import           Data.Binary                             (Binary)
import qualified Data.Binary                             as B
import           Data.ByteString.Lazy                    (ByteString)

import qualified Codec.Compression.BZip                  as BZ

import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

type CompressedOccurrences = OccOSerialized

-- ----------------------------------------------------------------------------

newtype OccOSerialized = OccOBs { unOccOBs :: ByteString }
  deriving (Eq, Show, NFData)

mkOccOBs :: ByteString -> OccOSerialized
mkOccOBs b = OccOBs $! b

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
  compressOcc           = mkOccOBs . BZ.compress . B.encode
  decompressOcc         = B.decode . BZ.decompress . unOccOBs
  differenceWithKeySet  = undefined

-- ----------------------------------------------------------------------------

instance Binary OccOSerialized where
  put = B.put . unOccOBs
  get = B.get >>= return . OccOBs

-- ----------------------------------------------------------------------------
