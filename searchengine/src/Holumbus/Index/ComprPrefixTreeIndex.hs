{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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

import qualified Data.StringMap.Strict                   as SM

import           Holumbus.Common.BasicTypes              (TextSearchOp (..))
import qualified Holumbus.Common.DocIdMap                as DM
import           Holumbus.Common.Occurrences             (Occurrences)
import           Holumbus.Common.Occurrences.Compression

import           Holumbus.Utility
-- ----------------------------------------------------------------------------

newtype ComprOccPrefixTree cv
    = ComprPT { comprPT :: SM.StringMap cv}
    deriving (Eq, Show, NFData)

mkComprPT :: NFData cv => SM.StringMap cv -> ComprOccPrefixTree cv
mkComprPT cv = ComprPT $! cv

-- ----------------------------------------------------------------------------

instance (NFData v, Binary v) => Binary (ComprOccPrefixTree v) where
    put (ComprPT i) = put i
    get = get >>= return . mkComprPT

-- ----------------------------------------------------------------------------

instance Index ComprOccPrefixTree where
    type IKey ComprOccPrefixTree v = SM.Key
    type IVal ComprOccPrefixTree v = Occurrences
    type ICon ComprOccPrefixTree v = (OccCompression v, NFData v)

    insert k v (ComprPT i)
        = mkComprPT $ SM.insert k (compressOcc v) i

    -- XXX not the best solution, but is there really another solution?
    batchDelete ks i
        = Ix.map (\m -> DM.diffWithSet m ks) i

    empty
        = mkComprPT $ SM.empty

    fromList l
        = mkComprPT . SM.fromList $ P.map (second compressOcc) l

    toList (ComprPT i)
        = second decompressOcc <$> SM.toList i

    search t k (ComprPT pt)
        = case t of
            Case         -> case SM.lookup k pt of
                              Nothing -> []
                              Just xs -> [(k, decompressOcc xs)]
            NoCase       -> luCase k pt
            PrefixCase   -> pfCase k pt
            PrefixNoCase -> pfNoCase k pt
        where
        toL f    = second decompressOcc <$> SM.toListShortestFirst f
        luCase   = toL .:: SM.lookupNoCase
        pfCase   = toL .:: SM.prefixFilter
        pfNoCase = toL .:: SM.prefixFilterNoCase


    lookupRange k1 k2 (ComprPT pt)
        = second decompressOcc <$> (SM.toList $ SM.lookupRange k1 k2 pt)

    unionWith op (ComprPT i1) (ComprPT i2)
        = mkComprPT $ SM.unionWith (\o1 o2 -> compressOcc $ op (decompressOcc o1) (decompressOcc o2)) i1 i2

    map f (ComprPT i)
        = mkComprPT $ SM.map (compressOcc . f . decompressOcc) i

    keys (ComprPT i)
        = SM.keys i
