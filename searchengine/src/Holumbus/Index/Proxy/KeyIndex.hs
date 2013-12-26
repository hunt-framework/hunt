{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Holumbus.Index.Proxy.KeyIndex
( KeyProxyIndex(..)
)
where

import           Prelude                                    as P
import           Control.DeepSeq

import           Control.Applicative                        ((<$>))
import           Control.Arrow                              (first)

import           Data.Binary                                (Binary (..))
import           Data.Bijection
import           Data.Text                                  (Text, pack, unpack)

import           Holumbus.Index.Index
import qualified Holumbus.Index.Index                       as Ix

import           Holumbus.Index.Proxy.CompressedIndex

import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

newtype KeyProxyIndex from impl cv
    = KPIx { kpIx :: impl cv}
    deriving (Eq, Show, NFData)

mkKPIx :: impl cv -> KeyProxyIndex from impl cv
mkKPIx v = KPIx $! v

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (KeyProxyIndex from impl v) where
    put = put . kpIx
    get = get >>= return . mkKPIx

-- ----------------------------------------------------------------------------

instance Index (KeyProxyIndex from impl) where
    type IKey      (KeyProxyIndex from impl) v = from
    type IVal      (KeyProxyIndex from impl) v = IVal impl v
    type ISearchOp (KeyProxyIndex from impl) v = ISearchOp impl v
    type ICon      (KeyProxyIndex from impl) v =
        ( Index impl
        , ICon impl v
        , Bijection from (IKey impl v)
        )

    insert k v (KPIx i)
        = mkKPIx $ insert (to k) v i

    batchDelete ks (KPIx i)
        = mkKPIx $ batchDelete ks i

    empty
        = mkKPIx $ empty

    fromList l
        = mkKPIx . fromList $ P.map (first to) l

    toList (KPIx i)
        = first from <$> toList i

    search t k (KPIx i)
        = first from <$> search t (to k) i

    lookupRange k1 k2 (KPIx i)
        = first from <$> lookupRange (to k1) (to k2) i

    unionWith op (KPIx i1) (KPIx i2)
        = mkKPIx $ unionWith op i1 i2

    map f (KPIx i)
        = mkKPIx $ Ix.map f i

    keys (KPIx i)
        = P.map from $ keys i


-- special instance for a CompressedOccurrences proxy within a TextKey proxy
-- This requires XFlexibleInstances
-- This requires XOverlappingInstances since the previous instance definition is more generic
-- TODO: can this be somehow generalized to a genric index containing a compression proxy?
instance Index (KeyProxyIndex from (ComprOccIndex impl to)) where
    type IKey      (KeyProxyIndex from (ComprOccIndex impl to)) v = from
    type IVal      (KeyProxyIndex from (ComprOccIndex impl to)) v = IVal      (ComprOccIndex impl to) v
    type ISearchOp (KeyProxyIndex from (ComprOccIndex impl to)) v = ISearchOp (ComprOccIndex impl to) v
    type ICon      (KeyProxyIndex from (ComprOccIndex impl to)) v =
        ( Index (ComprOccIndex impl to)
        , ICon  (ComprOccIndex impl to) v
        , Bijection from (IKey (ComprOccIndex impl to) v)
        )
    -- this is the only "special" function
    batchDelete docIds (KPIx (ComprIx pt))
        = mkKPIx $ mkComprIx $ Ix.map (differenceWithKeySet docIds) pt

    -- everything below is copied from the more general instance Index (KeyProxyIndex impl)
    insert k v (KPIx i)
        = mkKPIx $ insert (to k) v i

    empty
        = mkKPIx $ empty

    fromList l
        = mkKPIx . fromList $ P.map (first to) l

    toList (KPIx i)
        = first from <$> toList i

    search t k (KPIx i)
        = first from <$> search t (to k) i

    lookupRange k1 k2 (KPIx i)
        = first from <$> lookupRange (to k1) (to k2) i

    unionWith op (KPIx i1) (KPIx i2)
        = mkKPIx $ unionWith op i1 i2

    map f (KPIx i)
        = mkKPIx $ Ix.map f i

    keys (KPIx i)
        = P.map from $ keys i
