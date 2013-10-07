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

module Holumbus.Index.Common.Compression
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
  , differenceWithKeySet
  , differenceWithKeyList
  )
where

import qualified Data.IntSet                       as IS

import           Holumbus.Index.Common.DiffList
import           Holumbus.Index.Common.DocId       (DocId)
import           Holumbus.Index.Common.DocIdMap    (DocIdMap)
import qualified Holumbus.Index.Common.DocIdMap    as DM
import           Holumbus.Index.Common.Occurrences hiding (delete)

-- ----------------------------------------------------------------------------

-- | Compressed occurrences using a difference list implementation.
type CompressedOccurrences      = DocIdMap CompressedPositions
-- | Compressed positions using a difference list implementation.
type CompressedPositions        = DiffList

-- ----------------------------------------------------------------------------
class OccCompression cv where
    compress   :: Occurrences -> cv
    decompress :: cv -> Occurrences

instance OccCompression CompressedOccurrences where
    compress = deflateOcc
    decompress = inflateOcc

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
differenceWithKeySet :: DM.DocIdSet -> CompressedOccurrences -> CompressedOccurrences
differenceWithKeySet = flip $ IS.foldr delete

-- | Convert the compressed differences back to a set of integers.
inflatePos :: CompressedPositions -> Positions
inflatePos = toPositions

-- | Save some memory on the positions by just saving their differences and compressing these.
deflatePos :: Positions -> CompressedPositions
deflatePos = fromPositions

-- ----------------------------------------------------------------------------
