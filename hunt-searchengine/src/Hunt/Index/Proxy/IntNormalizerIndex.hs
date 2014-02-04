{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Hunt.Index.Proxy.IntNormalizerIndex
(
  -- normalized text to int
  IntNormalizerIndex(..)
  -- normalized text to text with sign as leading 0/1
, IntAsTextNormalizerIndex(..)
)
where

import           Prelude                                    as P

import           Control.Applicative                        ((<$>))
import           Control.Arrow                              (first)
import           Control.DeepSeq
import           Control.Monad

import           Data.Binary                                (Binary (..))
import           Data.Text                                  (Text)

import           Hunt.Index.Index
import qualified Hunt.Index.Index                       as Ix
import qualified Hunt.Index.Schema.Normalize.Int        as Int

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
    put = put . intNIx
    get = get >>= return . mkIntNIx

-- ----------------------------------------------------------------------------

-- | NOTE: Validation need to be performed before this proxy is applied
instance Index (IntNormalizerIndex impl) where
    type IKey      (IntNormalizerIndex impl) v = Text
    type IVal      (IntNormalizerIndex impl) v = IVal impl v
    type ICon      (IntNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Int
        )

    insert k v (IntNIx i)
        = liftM mkIntNIx $ insert (Int.normalizeToInt k) v i

    batchDelete ks (IntNIx i)
        = liftM mkIntNIx $ batchDelete ks i

    empty
        = mkIntNIx $ empty

    fromList l
        = liftM mkIntNIx . fromList $ P.map (first Int.normalizeToInt) l

    toList (IntNIx i)
        = liftM (first Int.denormalizeFromInt <$>) $ toList i

    search t k (IntNIx i)
        = liftM (first Int.denormalizeFromInt <$>) $ search t (Int.normalizeToInt k) i

    lookupRange k1 k2 (IntNIx i)
        = liftM (first Int.denormalizeFromInt <$>) $ lookupRange (Int.normalizeToInt k1) (Int.normalizeToInt k2) i

    unionWith op (IntNIx i1) (IntNIx i2)
        = liftM mkIntNIx $ unionWith op i1 i2

    unionWithConv to f (IntNIx i1) (IntNIx i2)
        = liftM mkIntNIx $ unionWithConv to f i1 i2

    map f (IntNIx i)
        = liftM mkIntNIx $ Ix.map f i

    keys (IntNIx i)
        = liftM (P.map Int.denormalizeFromInt) $ keys i


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
    type ICon      (IntAsTextNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Text
        )

    insert k v (IntAsTextNIx i)
        = liftM mkIntAsTextNIx $ insert (Int.normalizeToText k) v i

    batchDelete ks (IntAsTextNIx i)
        = liftM mkIntAsTextNIx $ batchDelete ks i

    empty
        = mkIntAsTextNIx $ empty

    fromList l
        = liftM mkIntAsTextNIx . fromList $ P.map (first Int.normalizeToText) l

    toList (IntAsTextNIx i)
        = liftM (first Int.denormalizeFromText <$>) $ toList i

    search t k (IntAsTextNIx i)
        = liftM (first Int.denormalizeFromText <$>) $ search t (Int.normalizeToText k) i

    lookupRange k1 k2 (IntAsTextNIx i)
        = liftM (first Int.denormalizeFromText <$>) $ lookupRange (Int.normalizeToText k1) (Int.normalizeToText k2) i

    unionWith op (IntAsTextNIx i1) (IntAsTextNIx i2)
        = liftM mkIntAsTextNIx $ unionWith op i1 i2

    unionWithConv to f (IntAsTextNIx i1) (IntAsTextNIx i2)
        = liftM mkIntAsTextNIx $ unionWithConv to f i1 i2

    map f (IntAsTextNIx i)
        = liftM mkIntAsTextNIx $ Ix.map f i

    keys (IntAsTextNIx i)
        = liftM (P.map Int.denormalizeFromText) $ keys i
