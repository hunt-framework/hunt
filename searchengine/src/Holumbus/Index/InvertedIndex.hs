{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Holumbus.Index.InvertedIndex
( InvertedIndex(..)
, InvertedIndexDate
, InvertedIndexInt
, InvertedIndexPosition
)
where

import           Prelude                                        as P

import           Control.DeepSeq
import           Control.Monad

import           Data.Bijection.Instances                       ()
import           Data.Binary                                    (Binary (..))
import           Data.Text                                      (Text)
import           Data.Typeable

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Occurrences                    (Occurrences)
import qualified Holumbus.Common.Occurrences                    as Occ
import           Holumbus.Common.Occurrences.Compression.Snappy

import           Holumbus.Index.ComprPrefixTreeIndex
import           Holumbus.Index.Index                           as Ix

import           Holumbus.Index.Proxy.DateNormalizerIndex
import           Holumbus.Index.Proxy.IntNormalizerIndex
import           Holumbus.Index.Proxy.KeyIndex
import           Holumbus.Index.Proxy.PositionNormalizerIndex

-- ----------------------------------------------------------------------------
-- inverted index using int proxy for numeric data
-- ----------------------------------------------------------------------------

newtype InvertedIndexInt v
    = InvIntIx { invIntIx :: IntAsTextNormalizerIndex InvertedIndex v }
    deriving (Eq, Show, NFData, Typeable)

mkInvIntIx :: IntAsTextNormalizerIndex InvertedIndex v -> InvertedIndexInt v
mkInvIntIx x = InvIntIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexInt v) where
    put = put . invIntIx
    get = get >>= return . InvIntIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexInt where
    type IKey InvertedIndexInt v = Word
    type IVal InvertedIndexInt v = Occurrences

    insert w o
        = do
            i' <- liftM mkInvIntIx $ insert w o empty
            unionWith Occ.merge i' -- InvIntIx $ insert w o i

    batchDelete docIds (InvIntIx i)
        = liftM mkInvIntIx $ batchDelete docIds i

    empty
        = mkInvIntIx $ empty

    fromList l
        = liftM mkInvIntIx $ fromList l

    toList (InvIntIx i)
        = toList i

    search t k (InvIntIx i)
        = search t k i

    lookupRange k1 k2 (InvIntIx i)
        = lookupRange k1 k2 i

    unionWith op (InvIntIx i1) (InvIntIx i2)
        = liftM mkInvIntIx $ unionWith op i1 i2

    map f (InvIntIx i)
        = liftM mkInvIntIx $ Ix.map f i

    keys (InvIntIx i)
        = keys i


-- ----------------------------------------------------------------------------
-- inverted index using date proxy for date information
-- ----------------------------------------------------------------------------

newtype InvertedIndexDate v
    = InvDateIx { invDateIx :: DateNormalizerIndex InvertedIndex v }
    deriving (Eq, Show, NFData, Typeable)

mkInvDateIx :: DateNormalizerIndex InvertedIndex v -> InvertedIndexDate v
mkInvDateIx x = InvDateIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexDate v) where
    put = put . invDateIx
    get = get >>= return . mkInvDateIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexDate where
    type IKey InvertedIndexDate v = Word
    type IVal InvertedIndexDate v = Occurrences

    insert w o
        = do
            i' <- liftM mkInvDateIx $ insert w o empty
            unionWith Occ.merge i' -- InvDateIx $ insert w o i

    batchDelete docIds (InvDateIx i)
        = liftM mkInvDateIx $ batchDelete docIds i

    empty
        = mkInvDateIx $ empty

    fromList l
        = liftM mkInvDateIx $ fromList l

    toList (InvDateIx i)
        = toList i

    search t k (InvDateIx i)
        = search t k i

    lookupRange k1 k2 (InvDateIx i)
        = lookupRange k1 k2 i

    unionWith op (InvDateIx i1) (InvDateIx i2)
        = liftM mkInvDateIx $ unionWith op i1 i2

    map f (InvDateIx i)
        = liftM mkInvDateIx $ Ix.map f i

    keys (InvDateIx i)
        = keys i

-- ----------------------------------------------------------------------------
-- inverted index using position proxy for geo coordinates
-- ----------------------------------------------------------------------------

newtype InvertedIndexPosition v
    = InvPosIx { invPosIx :: PositionNormalizerIndex  InvertedIndex v }
    deriving (Eq, Show, NFData, Typeable)

mkInvPosIx :: PositionNormalizerIndex InvertedIndex v -> InvertedIndexPosition v
mkInvPosIx x = InvPosIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexPosition v) where
    put = put . invPosIx
    get = get >>= return . mkInvPosIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexPosition where
    type IKey InvertedIndexPosition v = Word
    type IVal InvertedIndexPosition v = Occurrences

    insert w o
        = do
            i' <- liftM mkInvPosIx $ insert w o empty
            unionWith Occ.merge i' -- InvPosIx $ insert w o i

    batchDelete docIds (InvPosIx i)
        = liftM mkInvPosIx $ batchDelete docIds i

    empty
        = mkInvPosIx $ empty

    fromList l
        = liftM mkInvPosIx $ fromList l

    toList (InvPosIx i)
        = toList i

    search t k (InvPosIx i)
        = search t k i

    lookupRange k1 k2 (InvPosIx i)
        = lookupRange k1 k2 i

    unionWith op (InvPosIx i1) (InvPosIx i2)
        = liftM mkInvPosIx $ unionWith op i1 i2

    map f (InvPosIx i)
        = liftM mkInvPosIx $ Ix.map f i

    keys (InvPosIx i)
        = keys i


-- ----------------------------------------------------------------------------
-- default inverted index using text key
-- ----------------------------------------------------------------------------

newtype InvertedIndex _v
    = InvIx { invIx :: KeyProxyIndex Text ComprOccPrefixTree CompressedOccurrences }
    deriving (Eq, Show, NFData, Typeable)

mkInvIx :: KeyProxyIndex Text ComprOccPrefixTree CompressedOccurrences
        -> InvertedIndex v_
mkInvIx x = InvIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndex v) where
    put = put . invIx
    get = get >>= return . mkInvIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndex where
    type IKey InvertedIndex v = Word
    type IVal InvertedIndex v = Occurrences

    insert w o
        = do
            i' <- liftM mkInvIx $ insert w o empty
            unionWith Occ.merge i' -- InvIx $ insert w o i

    batchDelete docIds (InvIx i)
        = liftM mkInvIx $ batchDelete docIds i

    empty
        = mkInvIx $ empty

    fromList l
        = liftM mkInvIx $ fromList l

    toList (InvIx i)
        = toList i

    search t k (InvIx i)
        = search t k i

    lookupRange k1 k2 (InvIx i)
        = lookupRange k1 k2 i

    unionWith op (InvIx i1) (InvIx i2)
        = liftM mkInvIx $ unionWith op i1 i2

    map f (InvIx i)
        = liftM mkInvIx $ Ix.map f i

    keys (InvIx i)
        = keys i
