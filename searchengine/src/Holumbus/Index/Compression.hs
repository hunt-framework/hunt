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

module Holumbus.Index.Compression
  (
  -- * Compression types
  CompressedOccurrences
  , CompressedPositions

  -- * Compress
  , deflateOcc
  , deflatePos

  -- * Decompress
  , inflateOcc
  , inflatePos
  -- * Efficiency
  , delete
  , differenceWithKeySet
  )
where

import           Data.Set                         (Set)
import qualified Data.Set                         as S

import           Holumbus.Index.Common.DiffList
import           Holumbus.Index.Common.DocId      (DocId)
import           Holumbus.Index.Common.DocIdMap   (DocIdMap)
import qualified Holumbus.Index.Common.DocIdMap   as DM
import           Holumbus.Index.Common.Occurences hiding (delete)

-- ----------------------------------------------------------------------------

type CompressedOccurrences      = DocIdMap CompressedPositions
type CompressedPositions        = DiffList

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


-- | Difference without deflating and inflating.

differenceWithKeySet :: Set DocId -> CompressedOccurrences -> CompressedOccurrences
differenceWithKeySet = flip $ S.foldr delete

-- | Convert the compressed differences back to a set of integers.

inflatePos :: CompressedPositions -> Positions
inflatePos = toPositions

-- | Save some memory on the positions by just saving their differences and compressing these.

deflatePos :: Positions -> CompressedPositions
deflatePos = fromPositions

-- ----------------------------------------------------------------------------
