{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Hunt.Index.Proxy.PositionNormalizerIndex
(
  -- normalized position in textual representation to internal format
  PositionNormalizerIndex(..)
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
import qualified Hunt.Index.Schema.Normalize.Position   as Pos

-- ----------------------------------------------------------------------------
-- Int values represented as Int (Text to Int conversion)
-- ----------------------------------------------------------------------------

newtype PositionNormalizerIndex impl cv
    = PosNIx { posNIx :: impl cv}
    deriving (Eq, Show, NFData)

mkPosNIx :: impl cv -> PositionNormalizerIndex impl cv
mkPosNIx v = PosNIx $! v

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (PositionNormalizerIndex impl v) where
    put = put . posNIx
    get = get >>= return . mkPosNIx

-- ----------------------------------------------------------------------------

-- | NOTE: Validation need to be performed before this proxy is applied
instance Index (PositionNormalizerIndex impl) where
    type IKey      (PositionNormalizerIndex impl) v = Text
    type IVal      (PositionNormalizerIndex impl) v = IVal impl v
    type ICon      (PositionNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Text
        )

    batchInsert kvs (PosNIx i)
        = liftM mkPosNIx $ batchInsert (P.map (first Pos.normalize) kvs) i

    batchDelete ks (PosNIx i)
        = liftM mkPosNIx $ batchDelete ks i

    empty
        = mkPosNIx $ empty

    fromList l
        = liftM mkPosNIx . fromList $ P.map (first Pos.normalize) l

    toList (PosNIx i)
        = liftM (first Pos.denormalize <$>) $ toList i

    search t k (PosNIx i)
        = liftM (first Pos.denormalize <$>) $ search t (Pos.normalize k) i

    lookupRange k1 k2 (PosNIx i)
        = liftM (first Pos.denormalize <$>) $ lookupRange (Pos.normalize k1) (Pos.normalize k2) i

    unionWith op (PosNIx i1) (PosNIx i2)
        = liftM mkPosNIx $ unionWith op i1 i2

    unionWithConv to f (PosNIx i1) (PosNIx i2)
        = liftM mkPosNIx $ unionWithConv to f i1 i2

    map f (PosNIx i)
        = liftM mkPosNIx $ Ix.map f i

    keys (PosNIx i)
        = liftM (P.map Pos.denormalize) $ keys i

-- ----------------------------------------------------------------------------
