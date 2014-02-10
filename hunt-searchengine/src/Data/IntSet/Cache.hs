module Data.IntSet.Cache
where

import           Control.DeepSeq

import qualified Data.IntSet     as S
import qualified Data.Vector     as V

-- ------------------------------------------------------------
--
-- cache for single element IntSets with elements 0 <= i < cacheSize
--
-- These single element sets occur very frequently in position sets
-- in occurrence maps, so sharing becomes available with this cache.
--
-- Usage: substitute all singleton calls with cacheAt calls
-- and all fromList calls with @IS.unions . map cacheAt@

cacheSize :: Int
cacheSize = 1024

cache :: V.Vector S.IntSet
cache = id $!! V.generate cacheSize S.singleton

cacheAt :: Int -> S.IntSet
cacheAt i
    | 0 <= i
      &&
      i < cacheSize
          = id $! cache V.! i
    | otherwise
        = id $! S.singleton i

single0 :: S.IntSet
single0 = cacheAt 0

single1 :: S.IntSet
single1 = cacheAt 1

-- ------------------------------------------------------------



