{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- ----------------------------------------------------------------------------

{- |
  A index prototype with a deletion cache (size == 100 documents).
  Deletes are cached in a set and query results are filtered.
  This improves delete performance significantly but adds overhead to queries.

  The impact on queries increases the more documents are deleted. This can also affect query results
  in unexpected ways. To limit this effect, the cache is limited to 100 docs after which the
  deletion is applied.

  Snapshot support could be added by adding another index and searching both.
-}

-- ----------------------------------------------------------------------------

module Hunt.Index.Proxy.CachedIndex
  ( CachedIndex (..)
  )
where

import qualified Prelude                             as P

import           Control.Applicative                 hiding (empty)
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad

import           Data.Binary                         (Binary (..))
import qualified Data.IntSet                         as IS

import           Hunt.Common.DocIdMap                (DocIdSet)
import           Hunt.Common.Occurrences.Compression
import           Hunt.Index                          as Ix

import           Prelude                             as P



-- ------------------------------------------------------------

-- | An index with a deletion cache. A set of deleted 'DocId's is kept to filter the query results.
data CachedIndex impl v = CachedIx
  { cachedIds :: DocIdSet
  , cachedIx  :: ! (impl v)
  }
  deriving (Eq, Show)

-- | Wrap an index with the cache proxy.
mkCachedIx :: DocIdSet -> impl v -> CachedIndex impl v
mkCachedIx v = CachedIx $! v

-- ------------------------------------------------------------

instance NFData (CachedIndex impl v) where
  -- default

-- ------------------------------------------------------------

instance Binary (impl v) => Binary (CachedIndex impl v) where
  put (CachedIx c i) = put c >> put i
  get = mkCachedIx <$> get <*> get

-- ------------------------------------------------------------

instance Index (CachedIndex impl) where
  type IKey (CachedIndex impl) v = IKey impl v
  type IVal (CachedIndex impl) v = IVal impl v
  type ICon (CachedIndex impl) v
    = ( Index impl
      , ICon impl v
      , OccCompression (IVal impl v)
      )

  insertList op kvs (CachedIx c i)
    = mkCachedIx c (insertList op kvs i)

  deleteDocs ks (CachedIx c i)
    = if IS.size newSet > 100 -- merge/apply when cache size > 100
      then flatten newIx
      else newIx
    where
    newIx  = mkCachedIx newSet i
    newSet = IS.union c ks

  empty
    = mkCachedIx IS.empty empty

  fromList l
    = mkCachedIx IS.empty (fromList l)

  toList i
    = let (CachedIx _ i') = flatten i
      in toList i'

  search t k (CachedIx c i)
      = filterResult c $ search t k i

  lookupRange k1 k2 (CachedIx c i)
      = filterResult c $ lookupRange k1 k2 i

  unionWith op (CachedIx c1 i1) (CachedIx c2 i2)
      = mkCachedIx (IS.union c1 c2) (unionWith op i1 i2)

  unionWithConv to f (CachedIx c1 i1) (CachedIx c2 i2)
      = mkCachedIx (IS.union c1 c2) (unionWithConv to f i1 i2)

  map f i
    = let (CachedIx c i') = flatten i
      in mkCachedIx c (Ix.map f i')

  mapMaybe f i
    = let (CachedIx c i') = flatten i
      in mkCachedIx c (Ix.mapMaybe f i')

  keys (CachedIx _c i)
    = keys i

-- ------------------------------------------------------------

-- | Filter 'DocId's from raw results.
filterResult :: OccCompression v => IS.IntSet -> [(d, v)] -> [(d, v)]
filterResult c = P.map (second (deleteIds c))
  where deleteIds = differenceWithKeySet

-- | Flush the cache and apply the deletions.
flatten :: (ICon impl v, Index impl) => CachedIndex impl v -> CachedIndex impl v
flatten (CachedIx c i) = mkCachedIx IS.empty $ deleteDocs c i

-- ------------------------------------------------------------
