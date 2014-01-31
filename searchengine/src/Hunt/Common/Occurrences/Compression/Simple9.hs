{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------
{-
  Occurrences compression on the Simple-9 encoding scheme.
-}
-- ----------------------------------------------------------------------------

module Hunt.Common.Occurrences.Compression.Simple9
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

import           Hunt.Common.DiffList
import           Hunt.Common.DocId                   (DocId)
import           Hunt.Common.DocIdMap                (DocIdMap)
import qualified Hunt.Common.DocIdMap                as DM
import           Hunt.Common.Occurrences             hiding (delete)
import           Hunt.Common.Occurrences.Compression
import           Hunt.Common.Positions               (Positions)

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
