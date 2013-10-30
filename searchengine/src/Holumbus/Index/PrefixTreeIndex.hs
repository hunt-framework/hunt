module Holumbus.Index.PrefixTreeIndex
( DmPrefixTree(..)
)
where

import           Data.Binary                       (Binary(..))

import           Holumbus.Data.PrefixTree          (PrefixTree)
import qualified Holumbus.Data.PrefixTree          as PT

import           Holumbus.Common.DocIdMap          as DM
import           Holumbus.Index.Index

-- ----------------------------------------------------------------------------

newtype DmPrefixTree v
    = DmPT (PrefixTree (DocIdMap v))
    deriving Show

-- ----------------------------------------------------------------------------

instance Binary v => Binary (DmPrefixTree v) where
    put (DmPT i) = put i
    get = get >>= return . DmPT

-- ----------------------------------------------------------------------------

instance Index DmPrefixTree where
    type IKey DmPrefixTree v = PT.Key
    type IVal DmPrefixTree v = DocIdMap v

    insert k v (DmPT pt)
        = DmPT $ PT.insert k v pt

    batchDelete ks (DmPT pt)
        = DmPT $ PT.map (\m -> DM.diffWithSet m ks) pt

    empty
        = DmPT $ PT.empty

    fromList
        = DmPT . PT.fromList

    toList (DmPT pt)
        = PT.toList pt

    -- TODO: use indextype parameter for real search
    search t k (DmPT pt)
        = case t of
            _ -> PT.prefixFindWithKey k pt

    unionWith op (DmPT pt1) (DmPT pt2)
        = DmPT $ PT.unionWith op pt1 pt2

    map f (DmPT pt)
        = DmPT $ PT.map f pt

    keys (DmPT pt)
        = PT.keys pt
