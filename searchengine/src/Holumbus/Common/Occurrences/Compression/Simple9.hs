{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Holumbus.Common.Occurrences.Compression.Simple9
  (
  -- * Compression types
    CompressedOccurrences
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
  , differenceWithKeyList
  )
where

import qualified Data.IntSet                             as IS

import           Holumbus.Common.DiffList
import           Holumbus.Common.DocId                   (DocId)
import           Holumbus.Common.DocIdMap                (DocIdMap)
import qualified Holumbus.Common.DocIdMap                as DM
import           Holumbus.Common.Occurrences             hiding (delete)
import           Holumbus.Common.Occurrences.Compression
import           Holumbus.Common.Positions               (Positions)

-- ----------------------------------------------------------------------------

-- | Compressed occurrences using a difference list implementation.
type CompressedOccurrences      = DocIdMap CompressedPositions
-- | Compressed positions using a difference list implementation.
type CompressedPositions        = DiffList

-- ----------------------------------------------------------------------------

instance OccCompression CompressedOccurrences where
    compressOcc          = deflateOcc
    decompressOcc        = inflateOcc
    differenceWithKeySet = differenceWithKeySet'

-- ----------------------------------------------------------------------------

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
