module Holumbus.Index.PrefixTreeIndex
( DmPrefixTree(..)
)
where

import qualified Data.StringMap                    as SM

import           Holumbus.Common.DocIdMap          as DM
import           Holumbus.Index.Index

-- ----------------------------------------------------------------------------

newtype DmPrefixTree v = DmPT (SM.StringMap (DocIdMap v))
    deriving Show

-- ----------------------------------------------------------------------------

instance Index DmPrefixTree where
    type IKey DmPrefixTree v = SM.Key
    type IVal DmPrefixTree v = DocIdMap v

    insert k v (DmPT pt)
        = DmPT $ SM.insert k v pt

    batchDelete ks (DmPT pt)
        = DmPT $ SM.map (\m -> DM.diffWithSet m ks) pt

    empty
        = DmPT $ SM.empty

    fromList
        = DmPT . SM.fromList

    toList (DmPT pt)
        = SM.toList pt

    -- TODO: use indextype parameter for real search
    search t k (DmPT pt)
        = case t of
            _ -> SM.prefixFindWithKey k pt

    unionWith op (DmPT pt1) (DmPT pt2)
        = DmPT $ SM.unionWith op pt1 pt2

    map f (DmPT pt)
        = DmPT $ SM.map f pt

    keys (DmPT pt)
        = SM.keys pt
