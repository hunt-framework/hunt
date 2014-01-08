{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Holumbus.Index.Proxy.CompressedIndex
( ComprOccIndex (..)
, mkComprIx
)
where

import           Prelude                                 as P

import           Control.Applicative                     ((<$>))
import           Control.Arrow                           (second)
import           Control.DeepSeq

import           Data.Binary                             (Binary (..))

import           Holumbus.Index.Index
import qualified Holumbus.Index.Index                    as Ix

import           Holumbus.Common.Occurrences             (Occurrences)
import           Holumbus.Common.Occurrences.Compression

-- ----------------------------------------------------------------------------

newtype ComprOccIndex impl to from
    = ComprIx { comprIx :: impl to }
    deriving (Eq, Show, NFData)

mkComprIx :: impl to -> ComprOccIndex impl to from
mkComprIx v = ComprIx $! v

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (ComprOccIndex impl v from) where
    put = put . comprIx
    get = get >>= return . mkComprIx

-- ----------------------------------------------------------------------------

instance Index (ComprOccIndex impl to) where
    type IKey      (ComprOccIndex impl to) from = IKey impl to
    type IVal      (ComprOccIndex impl to) from = Occurrences
    type ICon      (ComprOccIndex impl to) from =
        ( Index impl
        , ICon impl to
        , OccCompression (IVal impl to)
        )

    insert k v (ComprIx i)
        = mkComprIx $ insert k (compressOcc v) i

    batchDelete ks (ComprIx i)
        = mkComprIx $ batchDelete ks i

    empty
        = mkComprIx $ empty

    fromList l
        = mkComprIx . fromList $ P.map (second compressOcc) l

    toList (ComprIx i)
        = second decompressOcc <$> toList i

    search t k (ComprIx i)
        = second decompressOcc <$> search t k i

    lookupRange k1 k2 (ComprIx i)
        = second decompressOcc <$> lookupRange k1 k2 i

    unionWith op (ComprIx i1) (ComprIx i2)
        = mkComprIx $ unionWith (\o1 o2 -> compressOcc $ op (decompressOcc o1) (decompressOcc o2)) i1 i2

    map f (ComprIx i)
        = mkComprIx $ Ix.map (compressOcc . f . decompressOcc) i

    keys (ComprIx i)
        = keys i
