{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Hunt.Index.PrefixTreeIndex
( DmPrefixTree(..)
)
where

import           Control.DeepSeq

import           Data.Binary                (Binary (..))
import           Data.Typeable

import qualified Data.StringMap.Strict      as SM

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdMap   as DM
import           Hunt.Index.Index

import           Hunt.Utility

-- ----------------------------------------------------------------------------

newtype DmPrefixTree v
    = DmPT { dmPT :: SM.StringMap (DocIdMap v) }
    deriving (Eq, Show, NFData, Typeable)

mkDmPT :: NFData v => SM.StringMap (DocIdMap v) -> DmPrefixTree v
mkDmPT v = DmPT $! v

-- ----------------------------------------------------------------------------

instance (NFData v,Binary v) => Binary (DmPrefixTree v) where
    put = put . dmPT
    get = get >>= return . mkDmPT

-- ----------------------------------------------------------------------------

instance Index DmPrefixTree where
    type IKey DmPrefixTree v = SM.Key
    type IVal DmPrefixTree v = DocIdMap v

    insert k v (DmPT pt)
        = return . mkDmPT $ SM.insert k v pt

    batchDelete ks (DmPT pt)
        = return . mkDmPT $ SM.map (\m -> DM.diffWithSet m ks) pt

    empty
        = mkDmPT $ SM.empty

    fromList
        = return . mkDmPT . SM.fromList

    toList (DmPT pt)
        = return $ SM.toList pt

    search t k (DmPT pt)
        = return $ case t of
            Case         -> case SM.lookup k pt of
                              Nothing -> []
                              Just xs -> [(k,xs)]
            NoCase       -> luCase k pt
            PrefixCase   -> pfCase k pt
            PrefixNoCase -> pfNoCase k pt
        where
        toL      = SM.toListShortestFirst
        luCase   = toL .:: SM.lookupNoCase
        pfCase   = toL .:: SM.prefixFilter
        pfNoCase = toL .:: SM.prefixFilterNoCase

    lookupRange k1 k2 (DmPT pt)
        = return . SM.toList $ SM.lookupRange k1 k2 pt

    unionWith op (DmPT pt1) (DmPT pt2)
        = return . mkDmPT $ SM.unionWith op pt1 pt2

    map f (DmPT pt)
        = return . mkDmPT $ SM.map f pt

    keys (DmPT pt)
        = return $ SM.keys pt
