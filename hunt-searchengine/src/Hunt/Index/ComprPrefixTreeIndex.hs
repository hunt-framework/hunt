{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Hunt.Index.ComprPrefixTreeIndex
( ComprOccPrefixTree(..)
)
where

import           Prelude                             as P

import           Control.Applicative                 ((<$>))
import           Control.Arrow                       (second)
import           Control.DeepSeq

import           Data.Binary                         (Binary (..))
import           Data.Typeable

import           Hunt.Index.Index
import qualified Hunt.Index.Index                    as Ix

import qualified Data.StringMap.Strict               as SM

import           Hunt.Common.BasicTypes              (TextSearchOp (..))
import qualified Hunt.Common.DocIdMap                as DM
import           Hunt.Common.Occurrences             (Occurrences)
import qualified Hunt.Common.Occurrences             as Occ
import           Hunt.Common.Occurrences.Compression

import           Hunt.Utility

-- ----------------------------------------------------------------------------

newtype ComprOccPrefixTree cv
  = ComprPT { comprPT :: SM.StringMap cv}
  deriving (Eq, Show, NFData, Typeable)

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

  -- FIXME: this is ugly
  -- a simple fromList does not work because there can be duplicates that need to be merged...
  batchInsert kos i1
    = unionWithConv compressOcc (\a b -> compressOcc (Occ.merge (decompressOcc a) b)) i1 ixs
    where
    ixs = foldr (unionWith Occ.merge) empty $ P.map (fromList . (:[])) kos

  -- XXX: not the best solution, but is there really another solution?
  batchDelete ks i
    = Ix.map (\m -> DM.diffWithSet m ks) i

  empty
    = mkComprPT $! SM.empty

  fromList l
    = mkComprPT $! SM.fromList $ P.map (second compressOcc) l

  toList (ComprPT i)
    = (second decompressOcc <$>) $ SM.toList i

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
    = second decompressOcc <$> (SM.toList . SM.lookupRange k1 k2 $ pt)

  unionWith op (ComprPT i1) (ComprPT i2)
    = mkComprPT $! SM.unionWith (\o1 o2 -> compressOcc $ op (decompressOcc o1) (decompressOcc o2)) i1 i2

  unionWithConv to f (ComprPT i1) (ComprPT i2)
    = mkComprPT $! SM.unionWithConv to f i1 i2

  map f (ComprPT i)
    = mkComprPT $! SM.map (compressOcc . f . decompressOcc) i

  keys (ComprPT i)
    = SM.keys i
