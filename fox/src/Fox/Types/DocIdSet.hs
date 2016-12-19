{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fox.Types.DocIdSet
  ( DocIdSet(..)
  , empty
  , singleton
  , null
  , size
  , member
  , fromList
  , toIntSet
  , toList
  , difference
  , union
  , intersection
  )
where

import           Fox.Types.Document (DocId (..))

import           Control.DeepSeq
import qualified Data.IntSet        as S
import qualified Data.List          as L
import           Prelude            hiding (null)

-- ------------------------------------------------------------
--
-- the wrapped DocId set

newtype DocIdSet = DIS { unDIS :: S.IntSet }
    deriving (Eq, Show, NFData)

instance Monoid DocIdSet where
    mempty
        = DIS S.empty
    mappend (DIS s1) (DIS s2)
        = DIS (S.union s1 s2)

empty :: DocIdSet
empty = DIS S.empty

difference :: DocIdSet -> DocIdSet -> DocIdSet
difference (DIS s1) (DIS s2) = DIS $ S.difference s1 s2

union :: DocIdSet -> DocIdSet -> DocIdSet
union (DIS s1) (DIS s2) = DIS $ S.union s1 s2

intersection :: DocIdSet -> DocIdSet -> DocIdSet
intersection (DIS s1) (DIS s2) = DIS $ S.intersection s1 s2

fromList :: [DocId] -> DocIdSet
fromList = DIS . S.fromList . L.map unDocId

toList :: DocIdSet -> [DocId]
toList = L.map DocId . S.toList . unDIS

toIntSet :: DocIdSet -> S.IntSet
toIntSet = unDIS

singleton :: DocId -> DocIdSet
singleton = DIS . S.singleton . unDocId

null :: DocIdSet -> Bool
null = S.null . unDIS

size :: DocIdSet -> Int
size (DIS s) = S.size s

member :: DocId -> DocIdSet -> Bool
member x s = unDocId x `S.member` unDIS s

