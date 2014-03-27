{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Hunt.Index.Proxy.KeyIndex
( KeyProxyIndex(..)
)
where

import           Prelude                             as P

import           Control.Applicative                 ((<$>))
import           Control.Arrow                       (first)
import           Control.DeepSeq

import           Data.Bijection
import           Data.Binary                         (Binary (..))

import qualified Hunt.Index                          as Ix
import           Hunt.Index

import           Hunt.Index.Proxy.CompressedIndex

import           Hunt.Common.Occurrences.Compression

-- ------------------------------------------------------------

newtype KeyProxyIndex toType impl cv
  = KPIx { kpIx :: impl cv }
  deriving (Eq, Show, NFData)

mkKPIx :: impl cv -> KeyProxyIndex toType impl cv
mkKPIx v = KPIx $! v

-- ------------------------------------------------------------

instance Binary (impl v) => Binary (KeyProxyIndex toType impl v) where
  put = put . kpIx
  get = get >>= return . mkKPIx

-- ------------------------------------------------------------

instance Index (KeyProxyIndex toType impl) where
  type IKey      (KeyProxyIndex toType impl) v = toType
  type IVal      (KeyProxyIndex toType impl) v = IVal impl v
  type ICon      (KeyProxyIndex toType impl) v =
    ( Index impl
    , ICon impl v
    , Bijection (IKey impl v) toType
    )

  insertList kvs (KPIx i)
    = mkKPIx $ insertList (P.map (first from) kvs) i

  deleteDocs ks (KPIx i)
    = mkKPIx $ deleteDocs ks i

  empty
    = mkKPIx $ empty

  fromList l
    = mkKPIx . fromList $ P.map (first from) l

  toList (KPIx i)
    = first to <$> toList i

  search t k (KPIx i)
    = first to <$> search t (from k) i

  lookupRange k1 k2 (KPIx i)
    = first to <$> lookupRange (from k1) (from k2) i

  unionWith op (KPIx i1) (KPIx i2)
    = mkKPIx $ unionWith op i1 i2

  unionWithConv t f (KPIx i1) (KPIx i2)
    = mkKPIx $ unionWithConv t f i1 i2

  map f (KPIx i)
    = mkKPIx $ Ix.map f i

  mapMaybe f (KPIx i)
    = mkKPIx $ Ix.mapMaybe f i

  keys (KPIx i)
    = P.map to $ keys i

-- special instance for a CompressedOccurrences proxy within a TextKey proxy
-- This requires XFlexibleInstances
-- This requires XOverlappingInstances since the previous instance definition is more generic
-- TODO: can this be somehow generalized to a genric index containing a compression proxy?
instance Index (KeyProxyIndex toType (ComprOccIndex impl to)) where
  type IKey      (KeyProxyIndex toType (ComprOccIndex impl to)) v = toType
  type IVal      (KeyProxyIndex toType (ComprOccIndex impl to)) v = IVal      (ComprOccIndex impl to) v
  type ICon      (KeyProxyIndex toType (ComprOccIndex impl to)) v =
    ( Index (ComprOccIndex impl to)
    , ICon  (ComprOccIndex impl to) v
    , Bijection (IKey (ComprOccIndex impl to) v) toType
    )

  -- this is the only "special" function
  deleteDocs docIds (KPIx (ComprIx pt))
    = mkKPIx $ mkComprIx $ Ix.map (differenceWithKeySet docIds) pt

  -- everything below is copied toType the more general instance Index (KeyProxyIndex impl)
  insertList kvs (KPIx i)
    = mkKPIx $ insertList (P.map (first from) kvs) i

  empty
    = mkKPIx $ empty

  fromList l
    = mkKPIx . fromList $ P.map (first from) l

  toList (KPIx i)
    = first to <$> toList i

  search t k (KPIx i)
    = first to <$> search t (from k) i

  lookupRange k1 k2 (KPIx i)
    = first to <$> lookupRange (from k1) (from k2) i

  unionWith op (KPIx i1) (KPIx i2)
    = mkKPIx $ unionWith op i1 i2

  unionWithConv t f (KPIx i1) (KPIx i2)
    = mkKPIx $ unionWithConv t f i1 i2

  map f (KPIx i)
    = mkKPIx $ Ix.map f i

  mapMaybe f (KPIx i)
    = mkKPIx $ Ix.mapMaybe f i

  keys (KPIx i)
    = P.map to $ keys i

-- ------------------------------------------------------------
