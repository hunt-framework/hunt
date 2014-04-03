{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- ----------------------------------------------------------------------------

{- |
  Key conversion proxy.
  Wraps an index to expose the desired key type.
  The conversion is defined by the 'Bijection' implementation.

  This can be used for simple conversions like @Text@ to @String@ or normalization and compression.
-}

-- ----------------------------------------------------------------------------

module Hunt.Index.Proxy.KeyIndex
( KeyProxyIndex (..)
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

-- | Key conversion proxy.
--   @toType@ is the desired/exposed key type, followed by the wrapped index type.
--   There has to be a corresponding 'Bijection' instance:
--
--   >  instance Bijection (IKey impl v) toType where ...
newtype KeyProxyIndex toType impl cv
  = KeyProxyIx { keyProxyIx :: impl cv }
  deriving (Eq, Show, NFData)

-- | Wrap an index in a key conversion proxy.
mkKeyProxyIx :: impl cv -> KeyProxyIndex toType impl cv
mkKeyProxyIx v = KeyProxyIx $! v

-- ------------------------------------------------------------

instance Binary (impl v) => Binary (KeyProxyIndex toType impl v) where
  put = put . keyProxyIx
  get = get >>= return . mkKeyProxyIx

-- ------------------------------------------------------------

instance Index (KeyProxyIndex toType impl) where
  type IKey      (KeyProxyIndex toType impl) v = toType
  type IVal      (KeyProxyIndex toType impl) v = IVal impl v
  type ICon      (KeyProxyIndex toType impl) v =
    ( Index impl
    , ICon impl v
    , Bijection (IKey impl v) toType
    )

  insertList kvs (KeyProxyIx i)
    = mkKeyProxyIx $ insertList (P.map (first from) kvs) i

  deleteDocs ks (KeyProxyIx i)
    = mkKeyProxyIx $ deleteDocs ks i

  empty
    = mkKeyProxyIx $ empty

  fromList l
    = mkKeyProxyIx . fromList $ P.map (first from) l

  toList (KeyProxyIx i)
    = first to <$> toList i

  search t k (KeyProxyIx i)
    = first to <$> search t (from k) i

  lookupRange k1 k2 (KeyProxyIx i)
    = first to <$> lookupRange (from k1) (from k2) i

  unionWith op (KeyProxyIx i1) (KeyProxyIx i2)
    = mkKeyProxyIx $ unionWith op i1 i2

  unionWithConv t f (KeyProxyIx i1) (KeyProxyIx i2)
    = mkKeyProxyIx $ unionWithConv t f i1 i2

  map f (KeyProxyIx i)
    = mkKeyProxyIx $ Ix.map f i

  mapMaybe f (KeyProxyIx i)
    = mkKeyProxyIx $ Ix.mapMaybe f i

  keys (KeyProxyIx i)
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
  deleteDocs docIds (KeyProxyIx (ComprIx pt))
    = mkKeyProxyIx $ mkComprIx $ Ix.map (differenceWithKeySet docIds) pt

  -- everything below is copied toType the more general instance Index (KeyProxyIndex impl)
  insertList kvs (KeyProxyIx i)
    = mkKeyProxyIx $ insertList (P.map (first from) kvs) i

  empty
    = mkKeyProxyIx $ empty

  fromList l
    = mkKeyProxyIx . fromList $ P.map (first from) l

  toList (KeyProxyIx i)
    = first to <$> toList i

  search t k (KeyProxyIx i)
    = first to <$> search t (from k) i

  lookupRange k1 k2 (KeyProxyIx i)
    = first to <$> lookupRange (from k1) (from k2) i

  unionWith op (KeyProxyIx i1) (KeyProxyIx i2)
    = mkKeyProxyIx $ unionWith op i1 i2

  unionWithConv t f (KeyProxyIx i1) (KeyProxyIx i2)
    = mkKeyProxyIx $ unionWithConv t f i1 i2

  map f (KeyProxyIx i)
    = mkKeyProxyIx $ Ix.map f i

  mapMaybe f (KeyProxyIx i)
    = mkKeyProxyIx $ Ix.mapMaybe f i

  keys (KeyProxyIx i)
    = P.map to $ keys i

-- ------------------------------------------------------------
