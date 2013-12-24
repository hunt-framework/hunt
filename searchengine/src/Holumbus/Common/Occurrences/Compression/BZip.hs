{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified Data.ByteString.Lazy                    as BL

import qualified Codec.Compression.BZip                  as BZ

import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

type CompressedOccurrences = OccOSerialized

-- ----------------------------------------------------------------------------

newtype OccOSerialized  = OccOBs { unOccOBs :: ByteString }
                          deriving (Eq, Show, NFData)

mkOccOBs :: ByteString -> OccOSerialized
mkOccOBs b = OccOBs $! b

-- ----------------------------------------------------------------------------

newtype ByteString      = Bs { unBs :: BL.ByteString }
                          deriving (Eq, Show)

mkBs                    :: BL.ByteString -> ByteString
mkBs s                  = Bs $!! s

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
