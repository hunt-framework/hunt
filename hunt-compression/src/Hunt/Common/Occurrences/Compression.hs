{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- ----------------------------------------------------------------------------

{- |
  Compression for 'Occurrences' with a simple type class 'OccCompression'.
  Different implementations are provided by the submodules.

  This also exports a strict 'Occurrences' version.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Occurrences.Compression
  (
  -- * Compression types
    OccCompression(..)
  , StrictOccurrences (..)
  )
where

import           Control.DeepSeq

import           Data.Binary
import           Data.Typeable

import qualified Hunt.Common.DocIdMap    as DocIdMap
import           Hunt.Common.DocIdSet    (DocIdSet)
import           Hunt.Common.Occurrences hiding (delete)

-- ------------------------------------------------------------

-- | Compression of 'Occurrences'.
--   Defines a bijection between compressed type @cv@ and 'Occurrences'.
class OccCompression cv where
  -- | Compress 'Occurrences'.
  compressOcc          :: Occurrences -> cv
  -- | Decompress 'Occurrences'.
  decompressOcc        :: cv -> Occurrences
  -- XXX: not sure if this is needed/used anymore
  -- | Delete a set of documents efficiently.
  --   Depending on the implementation, the compressed data type may not have to be decompressed.
  differenceWithKeySet :: DocIdSet -> cv -> cv

-- ------------------------------------------------------------

-- TODO: move to Occurrences.Strict?

-- | Strict Occurrences.
newtype StrictOccurrences
  = StrictOcc { unSOcc :: Occurrences }
  deriving (Eq, Show, Typeable, NFData)

-- | Make strict 'Occurrences' by fully evaluating it.
mkStrictOcc :: Occurrences -> StrictOccurrences
mkStrictOcc b = StrictOcc $!! b

-- ------------------------------------------------------------

instance Binary StrictOccurrences where
  put = put . unSOcc
  get = get >>= return . mkStrictOcc

instance OccCompression StrictOccurrences where
  compressOcc   = mkStrictOcc
  decompressOcc = unSOcc
  differenceWithKeySet s x = compressOcc $ DocIdMap.diffWithSet (decompressOcc x) s

instance OccCompression Occurrences where
  compressOcc   = id
  decompressOcc = id
  differenceWithKeySet = flip DocIdMap.diffWithSet

-- ------------------------------------------------------------
