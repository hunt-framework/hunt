{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Holumbus.Index.Proxy.IntNormalizerIndex
(
  -- normalized text to int
  IntNormalizerIndex(..)
  -- normalized text to text with sign as leading 0/1
, IntAsTextNormalizerIndex(..)
)
where

import           Prelude                                    as P
import           Control.DeepSeq

import           Control.Applicative                        ((<$>))
import           Control.Arrow                              (first)

import           Data.Binary                                (Binary (..))
import           Data.Text                                  (Text)

import           Holumbus.Index.Index
import qualified Holumbus.Index.Index                       as Ix
import qualified Holumbus.Index.Schema.Normalize.Int        as Int

-- ----------------------------------------------------------------------------
-- Int values represented as Int (Text to Int conversion)
-- ----------------------------------------------------------------------------

newtype IntNormalizerIndex impl cv
    = IntNIx { intNIx :: impl cv}
    deriving (Eq, Show, NFData)

mkIntNIx :: impl cv -> IntNormalizerIndex impl cv
mkIntNIx v = IntNIx $! v

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (IntNormalizerIndex impl v) where
    put (IntNIx i) = put i
    get = get >>= return . mkIntNIx

-- ----------------------------------------------------------------------------

-- | NOTE: Validation need to be performed before this proxy is applied
instance Index (IntNormalizerIndex impl) where
    type IKey      (IntNormalizerIndex impl) v = Text
    type IVal      (IntNormalizerIndex impl) v = IVal impl v
    type ISearchOp (IntNormalizerIndex impl) v = ISearchOp impl v
    type ICon      (IntNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Int
        )

    insert k v (IntNIx i)
        = mkIntNIx $ insert (Int.normalizeToInt k) v i

    batchDelete ks (IntNIx i)
        = mkIntNIx $ batchDelete ks i

    empty
        = mkIntNIx $ empty

    fromList l
        = mkIntNIx . fromList $ P.map (first Int.normalizeToInt) l

    toList (IntNIx i)
        = first Int.denormalizeFromInt <$> toList i

    search t k (IntNIx i)
        = first Int.denormalizeFromInt <$> search t (Int.normalizeToInt k) i

    lookupRange k1 k2 (IntNIx i)
        = first Int.denormalizeFromInt <$> lookupRange (Int.normalizeToInt k1) (Int.normalizeToInt k2) i

    unionWith op (IntNIx i1) (IntNIx i2)
        = mkIntNIx $ unionWith op i1 i2

    map f (IntNIx i)
        = mkIntNIx $ Ix.map f i

    keys (IntNIx i)
        = P.map Int.denormalizeFromInt $ keys i


-- ----------------------------------------------------------------------------
-- Int values represented as Text (Text to normalized Text conversion)
-- ----------------------------------------------------------------------------

newtype IntAsTextNormalizerIndex impl cv
    = IntAsTextNIx { intAsTextNIx :: impl cv}
    deriving (Eq, Show, NFData)

mkIntAsTextNIx :: impl cv -> IntAsTextNormalizerIndex impl cv
mkIntAsTextNIx v = IntAsTextNIx $! v

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (IntAsTextNormalizerIndex impl v) where
    put (IntAsTextNIx i) = put i
    get = get >>= return . mkIntAsTextNIx

-- ----------------------------------------------------------------------------

-- | NOTE: Validation need to be performed before this proxy is applied
instance Index (IntAsTextNormalizerIndex impl) where
    type IKey      (IntAsTextNormalizerIndex impl) v = Text
    type IVal      (IntAsTextNormalizerIndex impl) v = IVal impl v
    type ISearchOp (IntAsTextNormalizerIndex impl) v = ISearchOp impl v
    type ICon      (IntAsTextNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Text
        )

    insert k v (IntAsTextNIx i)
        = mkIntAsTextNIx $ insert (Int.normalizeToText k) v i

    batchDelete ks (IntAsTextNIx i)
        = mkIntAsTextNIx $ batchDelete ks i

    empty
        = mkIntAsTextNIx $ empty

    fromList l
        = mkIntAsTextNIx . fromList $ P.map (first Int.normalizeToText) l

    toList (IntAsTextNIx i)
        = first Int.denormalizeFromText <$> toList i

    search t k (IntAsTextNIx i)
        = first Int.denormalizeFromText <$> search t (Int.normalizeToText k) i

    lookupRange k1 k2 (IntAsTextNIx i)
        = first Int.denormalizeFromText <$> lookupRange (Int.normalizeToText k1) (Int.normalizeToText k2) i

    unionWith op (IntAsTextNIx i1) (IntAsTextNIx i2)
        = mkIntAsTextNIx $ unionWith op i1 i2

    map f (IntAsTextNIx i)
        = mkIntAsTextNIx $ Ix.map f i

    keys (IntAsTextNIx i)
        = P.map Int.denormalizeFromText $ keys i
