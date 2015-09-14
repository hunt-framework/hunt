{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.DocIdSet
  Copyright  : Copyright (C) 2014 Uwe Schmidt
  License    : MIT
  Maintainer : Uwe Schmidt
  Efficient Set implementation for 'DocId's.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.DocIdSet
  ( DocIdSet(..)
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

import           Prelude hiding (null)

import           Control.DeepSeq
import           Control.Monad

import           Data.Aeson
import           Data.Binary (Binary (..))
import qualified Data.IntSet as IS
import qualified Data.IntSet.Packed as S
import qualified Data.List as L
import           Data.Typeable
import           Hunt.Common.DocId

-- ------------------------------------------------------------
--
-- the wrapped DocId set

newtype DocIdSet = DIS { unDIS :: S.IntSet }
    deriving (Eq, Show, NFData, Typeable)

instance Binary DocIdSet where
  put = put . unDIS
  get = get >>= return . DIS

instance Monoid DocIdSet where
    mempty
        = DIS S.empty
    mappend (DIS s1) (DIS s2)
        = DIS (S.union s1 s2)

instance ToJSON DocIdSet where
    toJSON = toJSON . L.map DocId . S.toList . unDIS

instance FromJSON DocIdSet where
    parseJSON x = do l <- parseJSON x >>= fromL
                     return (DIS (S.fromList l))
        where
          fromL xs = forM xs $ \s ->
            case fromHex s of
              Nothing -> mzero
              Just i  -> return i

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

toIntSet :: DocIdSet -> IS.IntSet
toIntSet =  S.toIntSet . unDIS

singleton :: DocId -> DocIdSet
singleton = DIS . S.singleton . unDocId

null :: DocIdSet -> Bool
null = S.null . unDIS

size :: DocIdSet -> Int
size (DIS s) = S.size s

member :: DocId -> DocIdSet -> Bool
member x s = unDocId x `S.member` unDIS s
