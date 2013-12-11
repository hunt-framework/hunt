module Holumbus.Index.Proxy.CompressedIndex
( ComprOccIndex(..)
)
where

import           Prelude                                 as P

import           Control.Applicative                     ((<$>))
import           Control.Arrow                           (second)

import           Data.Binary                             (Binary (..))

import           Holumbus.Index.Index
import qualified Holumbus.Index.Index                    as Ix

import           Holumbus.Common.Occurrences             (Occurrences)
import           Holumbus.Common.Occurrences.Compression hiding (delete)

-- ----------------------------------------------------------------------------

newtype ComprOccIndex impl to from
    = ComprIx { comprIx :: impl to}
    deriving Show

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (ComprOccIndex impl v from) where
    put (ComprIx i) = put i
    get = get >>= return . ComprIx

-- ----------------------------------------------------------------------------

instance Index (ComprOccIndex impl to) where
    type IKey      (ComprOccIndex impl to) from = IKey impl to
    type IVal      (ComprOccIndex impl to) from = Occurrences
    type ISearchOp (ComprOccIndex impl to) from = ISearchOp impl to
    type ICon      (ComprOccIndex impl to) from =
        ( Index impl
        , ICon impl to
        , OccCompression (IVal impl to)
        )

    insert k v (ComprIx i)
        = ComprIx $ insert k (compressOcc v) i

    batchDelete ks (ComprIx i)
        = ComprIx $ batchDelete ks i

    empty
        = ComprIx $ empty

    fromList l
        = ComprIx . fromList $ P.map (second compressOcc) l

    toList (ComprIx i)
        = second decompressOcc <$> toList i

    search t k (ComprIx i)
        = second decompressOcc <$> search t k i

    lookupRange k1 k2 (ComprIx i)
        = second decompressOcc <$> lookupRange k1 k2 i

    unionWith op (ComprIx i1) (ComprIx i2)
        = ComprIx $ unionWith (\o1 o2 -> compressOcc $ op (decompressOcc o1) (decompressOcc o2)) i1 i2

    map f (ComprIx i)
        = ComprIx $ Ix.map (compressOcc . f . decompressOcc) i

    keys (ComprIx i)
        = keys i
