{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------

{- |
  Index compression proxy.
-}

-- ----------------------------------------------------------------------------

module Hunt.Index.Proxy.CompressedIndex
( ComprOccIndex (..)
, mkComprIx
)
where

import           Prelude                             as P

import           Control.Applicative                 ((<$>))
import           Control.Arrow                       (second)
import           Control.DeepSeq

import           Data.Binary                         (Binary (..))

import           Hunt.Common.Occurrences             (Occurrences)
import           Hunt.Common.Occurrences.Compression
import           Hunt.Index
import qualified Hunt.Index                          as Ix

-- ------------------------------------------------------------

-- | The index compression proxy.
newtype ComprOccIndex impl to from
  = ComprIx { comprIx :: impl to }
  deriving (Eq, Show, NFData)

-- | Wrap an index with the compression proxy.
mkComprIx :: impl to -> ComprOccIndex impl to from
mkComprIx v = ComprIx $! v

-- ------------------------------------------------------------

instance Binary (impl v) => Binary (ComprOccIndex impl v from) where
  put = put . comprIx
  get = get >>= return . mkComprIx

-- ------------------------------------------------------------

instance Index (ComprOccIndex impl to) where
  type IKey      (ComprOccIndex impl to) from = IKey impl to
  type IVal      (ComprOccIndex impl to) from = Occurrences
  type ICon      (ComprOccIndex impl to) from =
    ( Index impl
    , ICon impl to
    , OccCompression (IVal impl to)
    )
  insertList op kvs (ComprIx i)
      = mkComprIx $ insertList op' (P.map (second compressOcc) kvs) i
        where
          op' x y = compressOcc $ decompressOcc x `op` decompressOcc y

  deleteDocs ks (ComprIx i)
      = mkComprIx $ deleteDocs ks i

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
      = mkComprIx $ unionWith op' i1 i2
        where
          op' x y = compressOcc $ decompressOcc x `op` decompressOcc y

  map f (ComprIx i)
      = mkComprIx $ Ix.map (compressOcc . f . decompressOcc) i

  mapMaybe f (ComprIx i)
      = mkComprIx $ Ix.mapMaybe (fmap compressOcc . f . decompressOcc) i

  keys (ComprIx i)
      = keys i

-- ------------------------------------------------------------
