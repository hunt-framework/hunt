{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  Compressed 'StringMap' index.
  The value has to implement 'OccCompression'.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.ComprPrefixTreeIndex
( ComprOccPrefixTree (..)
)
where

import           Prelude                             as P

import           Control.Applicative                 ((<$>))
import           Control.Arrow                       (second)
import           Control.DeepSeq
import           Control.Parallel.Strategies

import           Data.Binary                         (Binary (..))
import           Data.Typeable

import           Hunt.Index
import qualified Hunt.Index                          as Ix

import qualified Data.StringMap.Strict               as SM

import           Hunt.Common.BasicTypes              (TextSearchOp (..))
import qualified Hunt.Common.DocIdMap                as DM
import           Hunt.Common.Occurrences             (Occurrences)
import           Hunt.Common.Occurrences.Compression

import           Hunt.Utility

-- ------------------------------------------------------------

-- | Compressed 'StringMap' index.  The value has to implement 'OccCompression'.
newtype ComprOccPrefixTree cv
  = ComprPT { comprPT :: SM.StringMap cv}
  deriving (Eq, Show, NFData, Typeable)

-- | Create a compressed 'StringMap' index.
mkComprPT :: NFData cv => SM.StringMap cv -> ComprOccPrefixTree cv
mkComprPT cv = ComprPT $! cv

-- ------------------------------------------------------------

instance (NFData v, Binary v) => Binary (ComprOccPrefixTree v) where
  put (ComprPT i) = put i
  get = get >>= return . mkComprPT

-- ------------------------------------------------------------
-- TODO: refactor

instance Index ComprOccPrefixTree where
  type IKey ComprOccPrefixTree v = SM.Key
  type IVal ComprOccPrefixTree v = Occurrences
  type ICon ComprOccPrefixTree v = (OccCompression v, NFData v)

  insertList op kos i
    = unionWithConv compressOcc (\a b -> compressOcc (op (decompressOcc a) b)) i ixs
    where
    ixs = if null m then empty else reduce m
       where
       m = parMap rpar (\ws -> P.foldr (unionWith op) empty $ P.map (fromList . (:[])) ws) (partitionListByLength 5000 kos)

       reduce mapRes
         = case parMap rpar (\(i1,i2) -> unionWith op i1 i2) $ mkPairs mapRes of
             []     -> error "insertList (ComprOccPrefixTree): impossible case"
             [x]    -> x
             xs     -> reduce xs

       mkPairs :: [a] -> [(a,a)]
       mkPairs []       = []
       mkPairs (a:[])   = [(a,a)]
       mkPairs (a:b:xs) = (a,b):mkPairs xs

    unionWithConv to f (ComprPT i1) (ComprPT i2)
      = mkComprPT $! SM.unionMapWith to f i1 i2

  -- XXX: not the best solution, but is there really another solution?
  deleteDocs ks i
    = Ix.mapMaybe (\m -> let dm = DM.diffWithSet m ks
                         in if DM.null dm then Nothing else Just dm) i

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



  map f (ComprPT i)
    = mkComprPT $! SM.map (compressOcc . f . decompressOcc) i

  mapMaybe f (ComprPT i)
    = mkComprPT $! SM.mapMaybe (fmap compressOcc . f . decompressOcc) i

  keys (ComprPT i)
    = SM.keys i

-- ------------------------------------------------------------

