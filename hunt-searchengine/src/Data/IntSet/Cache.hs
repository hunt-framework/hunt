-- ----------------------------------------------------------------------------
{- |
A cache for single element 'IntSet's with elements 0 <= i < cacheSize (1024)
These single element sets occur very frequently in position sets
in occurrence maps, so sharing becomes available with this cache.

Usage: substitute all singleton calls with @cacheAt@ calls
and all @fromList@ calls with @IS.unions . map cacheAt@.
-}
-- ----------------------------------------------------------------------------

module Data.IntSet.Cache
  ( cache
  , cacheAt
  )
where
import           Control.DeepSeq

import qualified Data.IntSet as S
import qualified Data.Vector as V

-- ------------------------------------------------------------

-- | Size of the cache.
cacheSize :: Int
cacheSize = 1024

-- | Initialize the 'IntSet' cache.
cache :: V.Vector S.IntSet
cache = id $!! V.generate cacheSize S.singleton

-- | A (cached) 'IntSet' singleton.
cacheAt :: Int -> S.IntSet
cacheAt i
    | 0 <= i
      &&
      i < cacheSize
          = id $! cache V.! i
    | otherwise
        = id $! S.singleton i

-- ------------------------------------------------------------
