{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Compression
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides several specific compression mechanisms for different
  parts of indexes. Right now, just a general compression scheme for
  the 'Occurrences' and 'Positions' are provided.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.Occurrences.Compression
  (
  -- * Compression types
    CompressedOccurrences
  , SerializedOccurrences
  , OccOSerialized
  , CompressedPositions
  , OccCompression(..)

  -- * Compress
  , deflateOcc
  , deflatePos

  -- * Decompress
  , inflateOcc
  , inflatePos
  -- * Efficiency
  , delete
  -- , differenceWithKeySet
  , differenceWithKeyList
  )
where

import           Control.DeepSeq
import qualified Data.Binary                 as B
import qualified Data.IntSet                 as IS
import qualified Codec.Compression.BZip      as BZ

import           Holumbus.Common.DiffList
import qualified Data.ByteString.Lazy       as BL


import           Holumbus.Common.DocId       (DocId)
import           Holumbus.Common.DocIdMap    (DocIdMap)
import qualified Holumbus.Common.DocIdMap    as DM
import           Holumbus.Common.Occurrences hiding (delete)
import           Holumbus.Common.Positions   (Positions)

-- ----------------------------------------------------------------------------

type SerializedOccurrences      = OccOSerialized


-- | Compressed occurrences using a difference list implementation.
type CompressedOccurrences      = DocIdMap CompressedPositions
-- | Compressed positions using a difference list implementation.
type CompressedPositions        = DiffList

-- ----------------------------------------------------------------------------
class OccCompression cv where
    compressOcc          :: Occurrences -> cv
    decompressOcc        :: cv -> Occurrences
    differenceWithKeySet :: DM.DocIdSet -> cv -> cv

instance OccCompression CompressedOccurrences where
    compressOcc          = deflateOcc
    decompressOcc        = inflateOcc
    differenceWithKeySet = differenceWithKeySet'


newtype OccOSerialized  = OccOBs { unOccOBs :: ByteString }
                          deriving (Eq, Show, NFData)

instance OccCompression SerializedOccurrences where
  compressOcc           = OccOBs . mkBs . BZ.compress . B.encode
  decompressOcc         = B.decode . BZ.decompress . unBs. unOccOBs
  differenceWithKeySet  = undefined


instance B.Binary OccOSerialized where
  put                   = B.put . unOccOBs
  get                   = B.get >>= return . OccOBs

newtype ByteString      = Bs { unBs :: BL.ByteString }
                          deriving (Eq, Show)

mkBs                    :: BL.ByteString -> ByteString
mkBs s                  = Bs $!! s

instance NFData ByteString where
-- use default implementation: eval to WHNF, and that's sufficient
--
instance B.Binary ByteString where
   put                   = B.put . unBs
   get                   = B.get >>= return . mkBs


-- | Decompressing the occurrences by just decompressing all contained positions.
inflateOcc :: CompressedOccurrences -> Occurrences
inflateOcc = DM.map inflatePos

-- | Compress the occurrences by just compressing all contained positions.
deflateOcc :: Occurrences -> CompressedOccurrences
deflateOcc = DM.map deflatePos

-- XXX: Maybe unnecessary due to lazy evaluation
-- | Delete without deflating and inflating.
delete :: DocId -> CompressedOccurrences -> CompressedOccurrences
delete = DM.delete

differenceWithKeyList :: [DocId] -> CompressedOccurrences -> CompressedOccurrences
differenceWithKeyList = differenceWithKeySet . IS.fromList

-- | Difference without deflating and inflating.
differenceWithKeySet' :: DM.DocIdSet -> CompressedOccurrences -> CompressedOccurrences
differenceWithKeySet' = flip $ IS.foldr delete

-- | Convert the compressed differences back to a set of integers.
inflatePos :: CompressedPositions -> Positions
inflatePos = toPositions

-- | Save some memory on the positions by just saving their differences and compressing these.
deflatePos :: Positions -> CompressedPositions
deflatePos = fromPositions

-- ----------------------------------------------------------------------------
