{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Compression
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

module Hunt.Common.Occurrences.Compression
  (
  -- * Compression types
    OccCompression(..)
  )
where

import qualified Hunt.Common.DocIdMap    as DM
import           Hunt.Common.Occurrences hiding (delete)

-- ----------------------------------------------------------------------------

class OccCompression cv where
    compressOcc          :: Occurrences -> cv
    decompressOcc        :: cv -> Occurrences
    -- XXX: not sure if this is needed/used anymore
    differenceWithKeySet :: DM.DocIdSet -> cv -> cv
