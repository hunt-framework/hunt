{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module Hunt.Index.Proxy.CachedIndex
where

import qualified Prelude                             as P

import           Control.Applicative                 hiding (empty)
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad

import qualified Data.IntSet                         as IS

import           Hunt.Common.DocIdMap                (DocIdSet)
--import qualified Hunt.Common.DocIdMap                as DM
import           Hunt.Common.Occurrences.Compression
import           Prelude                             as P

import           Data.Binary                         (Binary (..))

import           Hunt.Index.Index
import           Hunt.Index.Index                    as Ix

-- ----------------------------------------------------------------------------

data CachedIndex impl v = CachedIx
  { cachedIds :: DocIdSet
  , cachedIx  :: ! (impl v)
  }
  deriving (Eq, Show)

mkCachedIx :: DocIdSet -> impl v -> CachedIndex impl v
mkCachedIx v = CachedIx $! v


instance NFData (CachedIndex impl v) where
  -- default

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (CachedIndex impl v) where
  put (CachedIx c i) = put c >> put i
  get = mkCachedIx <$> get <*> get

-- ----------------------------------------------------------------------------

instance Index (CachedIndex impl) where
  type IKey (CachedIndex impl) v = IKey impl v
  type IVal (CachedIndex impl) v = IVal impl v
  type ICon (CachedIndex impl) v
    = ( Index impl
      , ICon impl v
      , OccCompression (IVal impl v)
      )

  insertList kvs (CachedIx c i)
    = mkCachedIx c (insertList kvs i)

  deleteDocs ks (CachedIx c i)
    = mkCachedIx (IS.union c ks) i

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
      = (mkCachedIx (IS.union c1 c2)) $ unionWithConv to f i1 i2

  map f i
    = let (CachedIx c i') = flatten i
      in mkCachedIx c (Ix.map f i')

  mapMaybe f i
    = let (CachedIx c i') = flatten i
      in mkCachedIx c (Ix.mapMaybe f i')

  keys (CachedIx _c i)
    = keys i

-- ----------------------------------------------------------------------------

filterResult :: OccCompression v => IS.IntSet -> [(d, v)] -> [(d, v)]
filterResult c = P.map (second (deleteIds c))
  where deleteIds = differenceWithKeySet

flatten :: (ICon impl v, Index impl) => CachedIndex impl v -> (CachedIndex impl v)
flatten (CachedIx c i) = (mkCachedIx IS.empty) $ (deleteDocs c i)
