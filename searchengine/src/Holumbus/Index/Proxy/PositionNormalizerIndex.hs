{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Holumbus.Index.Proxy.PositionNormalizerIndex
( 
  -- normalized position in textual representation to interal format
  PositionNormalizerIndex(..)
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
import qualified Holumbus.Index.Schema.Normalize.Position   as Pos

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
    put (PosNIx i) = put i
    get = get >>= return . mkPosNIx
 
         
-- ----------------------------------------------------------------------------

-- | NOTE: Validation need to be performed before this proxy is applied
instance Index (PositionNormalizerIndex impl) where
    type IKey      (PositionNormalizerIndex impl) v = Text
    type IVal      (PositionNormalizerIndex impl) v = IVal impl v
    type ISearchOp (PositionNormalizerIndex impl) v = ISearchOp impl v
    type ICon      (PositionNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Text
        )

    insert k v (PosNIx i)
        = mkPosNIx $ insert (Pos.normalize k) v i

    batchDelete ks (PosNIx i)
        = mkPosNIx $ batchDelete ks i

    empty
        = mkPosNIx $ empty

    fromList l
        = mkPosNIx . fromList $ P.map (first Pos.normalize) l

    toList (PosNIx i)
        = first Pos.denormalize <$> toList i

    search t k (PosNIx i)
        = first Pos.denormalize <$> search t (Pos.normalize k) i

    lookupRange k1 k2 (PosNIx i)
        = first Pos.denormalize <$> lookupRange (Pos.normalize k1) (Pos.normalize k2) i

    unionWith op (PosNIx i1) (PosNIx i2)
        = mkPosNIx $ unionWith op i1 i2

    map f (PosNIx i)
        = mkPosNIx $ Ix.map f i

    keys (PosNIx i)
        = P.map Pos.denormalize $ keys i

-- ----------------------------------------------------------------------------
