{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Holumbus.Index.ComprPrefixTreeIndex
( ComprOccPrefixTree(..)
)
where

import           Prelude                                 as P

import           Control.Applicative                     ((<$>))
import           Control.Arrow                           (second)
import           Control.DeepSeq

import           Data.Binary                             (Binary (..))

import           Holumbus.Index.Index
import qualified Holumbus.Index.Index                    as Ix
import           Holumbus.Index.PrefixTreeIndex

import           Holumbus.Common.DocIdMap                (DocIdMap)
import           Holumbus.Common.Occurrences             (Occurrences)
import           Holumbus.Common.Occurrences.Compression hiding (delete)

import qualified Data.StringMap                          as SM

-- ----------------------------------------------------------------------------

newtype ComprOccPrefixTree cv
    = ComprPT { comprPT :: DmPrefixTree cv}
    deriving (Eq, Show, NFData)

mkComprPT :: NFData cv => DmPrefixTree cv -> ComprOccPrefixTree cv
mkComprPT cv = ComprPT $! cv 

-- ----------------------------------------------------------------------------

instance (NFData v, Binary v) => Binary (ComprOccPrefixTree v) where
    put (ComprPT i) = put i
    get = get >>= return . mkComprPT

-- ----------------------------------------------------------------------------

instance Index ComprOccPrefixTree where
    type IKey ComprOccPrefixTree v = SM.Key
    type IVal ComprOccPrefixTree v = Occurrences
    type ICon ComprOccPrefixTree v = (OccCompression (DocIdMap v), NFData v)

    -- | why is this strict, even without using the smart constructor?
    insert k v (ComprPT i)
        = mkComprPT $ insert k (compressOcc v) i

    batchDelete ks (ComprPT i)
        = mkComprPT $ batchDelete ks i

    empty
        = mkComprPT $ empty

    fromList l
        = mkComprPT . fromList $ P.map (second compressOcc) l

    toList (ComprPT i)
        = second decompressOcc <$> toList i

    search t k (ComprPT i)
        = second decompressOcc <$> search t k i

    lookupRange k1 k2 (ComprPT i)
        = second decompressOcc <$> lookupRange k1 k2 i

    unionWith op (ComprPT i1) (ComprPT i2)
        = mkComprPT $ unionWith (\o1 o2 -> compressOcc $ op (decompressOcc o1) (decompressOcc o2)) i1 i2

    map f (ComprPT i)
        = mkComprPT $ Ix.map (compressOcc . f . decompressOcc) i

    keys (ComprPT i)
        = keys i
