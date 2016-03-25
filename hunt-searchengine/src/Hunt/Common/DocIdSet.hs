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
  , fromDistinctAscList
  , toIntSet
  , toList
  , difference
  , union
  , intersection
  , split
  )
where

import           Control.DeepSeq
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Binary (Binary (..))
import qualified Data.IntSet as IS
import qualified Data.IntSet.Packed as S
import qualified Data.List as L
import           Data.Typeable
import           Hunt.Common.DocId
import           Prelude hiding (null)

-- ------------------------------------------------------------
--
-- the wrapped DocId set

newtype DocIdSet = DIS { unDIS :: IS.IntSet }
    deriving (Eq, Monoid, Show, NFData, Typeable)

instance Binary DocIdSet where
  put = undefined -- put . unDIS
  get = undefined -- get >>= return . DIS
{-
instance Monoid DocIdSet where
    mempty
        = DIS S.empty
    {-# INLINE mempty #-}

    mappend (DIS s1) (DIS s2)
        = DIS (S.union s1 s2)
    {-# INLINE mappend #-}
-}

instance ToJSON DocIdSet where
    toJSON = undefined -- toJSON . L.map DocId . S.toList . unDIS

instance FromJSON DocIdSet where
    parseJSON x = undefined {- do l <- parseJSON x >>= fromL
                     return (DIS (S.fromList l))
        where
          fromL xs = forM xs $ \s ->
            case fromHex s of
              Nothing -> mzero
              Just i  -> return i
-}

difference :: DocIdSet -> DocIdSet -> DocIdSet
difference (DIS s1) (DIS s2) = DIS $ IS.difference s1 s2
{-# INLINE difference #-}

union :: DocIdSet -> DocIdSet -> DocIdSet
union (DIS s1) (DIS s2) = DIS $ IS.union s1 s2
{-# INLINE union #-}

intersection :: DocIdSet -> DocIdSet -> DocIdSet
intersection (DIS s1) (DIS s2) = DIS $ IS.intersection s1 s2
{-# INLINE intersection #-}

fromList :: [DocId] -> DocIdSet
fromList = DIS . IS.fromList . L.map unDocId
{-# INLINE fromList #-}

fromDistinctAscList :: [DocId] -> DocIdSet
fromDistinctAscList = DIS . IS.fromDistinctAscList . fmap unDocId
{-# INLINE fromDistinctAscList #-}

toList :: DocIdSet -> [DocId]
toList = fmap DocId . IS.toList . unDIS
{-# INLINE toList #-}

toIntSet :: DocIdSet -> IS.IntSet
toIntSet = unDIS
{-# INLINE toIntSet #-}

singleton :: DocId -> DocIdSet
singleton = DIS . IS.singleton . unDocId
{-# INLINE singleton #-}

null :: DocIdSet -> Bool
null = IS.null . unDIS
{-# INLINE null #-}

size :: DocIdSet -> Int
size (DIS s) = IS.size s
{-# INLINE size #-}

member :: DocId -> DocIdSet -> Bool
member x s = unDocId x `IS.member` unDIS s
{-# INLINE member #-}

split :: DocId -> DocIdSet -> (Maybe DocId, DocIdSet, DocIdSet)
split docId@(DocId did) (DIS s) =
  let
    (l, x, r) = IS.splitMember did s
  in (if x then Just docId else Nothing, DIS l, DIS r)
