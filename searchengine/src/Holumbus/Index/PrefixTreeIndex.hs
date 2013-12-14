{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Holumbus.Index.PrefixTreeIndex
( DmPrefixTree(..)
)
where

import           Control.DeepSeq
import           Data.Binary                       (Binary (..))

import qualified Data.StringMap.Strict             as SM

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.DocIdMap          as DM
import           Holumbus.Index.Index

-- ----------------------------------------------------------------------------

newtype DmPrefixTree v
    = DmPT (SM.StringMap (DocIdMap v))
    deriving (Eq, Show, NFData)

mkDmPT :: NFData v => SM.StringMap (DocIdMap v) -> DmPrefixTree v
mkDmPT v = DmPT $!! v

-- ----------------------------------------------------------------------------

instance (NFData v,Binary v) => Binary (DmPrefixTree v) where
    put (DmPT i) = put i
    get = get >>= return . mkDmPT

-- ----------------------------------------------------------------------------

instance Index DmPrefixTree where
    type IKey DmPrefixTree v = SM.Key
    type IVal DmPrefixTree v = DocIdMap v

    insert k v (DmPT pt)
        = mkDmPT $ SM.insert k v pt

    batchDelete ks (DmPT pt)
        = mkDmPT $ SM.map (\m -> DM.diffWithSet m ks) pt

    empty
        = mkDmPT $ SM.empty

    fromList
        = mkDmPT . SM.fromList

    toList (DmPT pt)
        = SM.toList pt

    search t k (DmPT pt)
        = case t of
            Case         -> case SM.lookup k pt of
                              Nothing   -> []
                              (Just xs) -> [(k,xs)]
            NoCase       -> SM.lookupNoCase k pt
            PrefixCase   -> SM.prefixFindCaseWithKey k pt
            PrefixNoCase -> SM.prefixFindNoCaseWithKey k pt

    lookupRange k1 k2 (DmPT pt)
        = SM.toList $ SM.lookupRange k1 k2 pt

    unionWith op (DmPT pt1) (DmPT pt2)
        = mkDmPT $ SM.unionWith op pt1 pt2

    map f (DmPT pt)
        = mkDmPT $ SM.map f pt

    keys (DmPT pt)
        = SM.keys pt
