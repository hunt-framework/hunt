{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.DocIdMap
  Copyright  : Copyright (C) 2012 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  DocId maps

-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.DocIdMap
    ( DocIdMap(..)
    , DocIdSet
    , empty
    , singleton
    , null
    , member
    , lookup
    , insert
    , delete
    , insertWith
    , size
    , minKey
    , maxKey
    , isIntervall
    , union
    , intersection
    , difference
    , diffWithSet
    , unionWith
    , intersectionWith
    , differenceWith
    , unionsWith
    , map
    , filter
    , filterWithKey
    , mapWithKey
    , traverseWithKey
    , foldr
    , foldrWithKey
    , fromList
    , fromAscList
    , toList
    , keys
    , elems
    , toDocIdSet
    )
where

import           Prelude                     hiding (filter, foldr, lookup, map,
                                              null)
import qualified Prelude                     as P

import           Control.Applicative         (Applicative(..), (<*>), pure)
import           Control.DeepSeq

import           Data.Binary                 (Binary (..))
import qualified Data.Binary                 as B
import           Data.Foldable               hiding (fold, foldr, toList)
import           Data.Traversable
import qualified Data.IntMap.Strict          as IM
import qualified Data.IntSet                 as S

import           Holumbus.Common.DocId
import qualified Holumbus.Common.DocId       as DId

-- ------------------------------------------------------------

type DocIdSet           = S.IntSet

toDocIdSet              :: [DocId] -> DocIdSet
toDocIdSet              = S.fromList

newtype DocIdMap v      = DIM { unDIM :: IM.IntMap v }
                          deriving
                          (Eq, Show, Foldable, Traversable, Functor, NFData)

liftDIM                 :: (IM.IntMap v -> IM.IntMap r) ->
                           DocIdMap v -> DocIdMap r
liftDIM f               = DIM . f . unDIM

liftDIM2                :: (IM.IntMap v -> IM.IntMap w -> IM.IntMap x) ->
                           DocIdMap v -> DocIdMap w -> DocIdMap x
liftDIM2 f x y          = DIM $ f (unDIM x) (unDIM y)

empty                   :: DocIdMap v
empty                   = DIM $ IM.empty

singleton               :: DocId -> v -> DocIdMap v
singleton d v           = insert d v empty

null                    :: DocIdMap v -> Bool
null                    = IM.null . unDIM

member                  :: DocId -> DocIdMap v -> Bool
member x                = IM.member x . unDIM

lookup                  :: DocId -> DocIdMap v -> Maybe v
lookup x                = IM.lookup x . unDIM

insert                  :: DocId -> v -> DocIdMap v -> DocIdMap v
insert x y              = liftDIM $ IM.insert x y

delete                  :: DocId -> DocIdMap v -> DocIdMap v
delete x                = liftDIM $ IM.delete x

insertWith              :: (v -> v -> v) -> DocId -> v -> DocIdMap v -> DocIdMap v
insertWith f x y        = liftDIM $ IM.insertWith f x y

size                    :: DocIdMap v -> Int
size                    = IM.size . unDIM

minKey                  :: DocIdMap v -> DocId
minKey                  = maybe DId.mkNull (fst . fst) . IM.minViewWithKey . unDIM

maxKey                  :: DocIdMap v -> DocId
maxKey                  = maybe DId.mkNull (fst . fst) . IM.maxViewWithKey . unDIM

isIntervall             :: DocIdMap v -> Bool
isIntervall m           = null m
                                  ||
                                  ( maxKey m - minKey m
                                    == size m - 1
                                  )

union                   :: DocIdMap v -> DocIdMap v -> DocIdMap v
union                   = liftDIM2 $ IM.union

intersection            :: DocIdMap v -> DocIdMap v -> DocIdMap v
intersection            = liftDIM2 $ IM.intersection

difference              :: DocIdMap v -> DocIdMap w -> DocIdMap v
difference              = liftDIM2 $ IM.difference

diffWithSet             :: DocIdMap v -> DocIdSet -> DocIdMap v
diffWithSet m s         = m `difference` (DIM $ IM.fromSet (const ()) s)

unionWith               :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
unionWith f             = liftDIM2 $ IM.unionWith f

intersectionWith        :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
intersectionWith f      = liftDIM2 $ IM.intersectionWith f

differenceWith          :: (v -> v -> Maybe v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
differenceWith f        = liftDIM2 $ IM.differenceWith f

unionsWith              :: (v -> v -> v) -> [DocIdMap v] -> DocIdMap v
unionsWith f            = DIM . IM.unionsWith f . P.map unDIM

map                     :: (v -> r) -> DocIdMap v -> DocIdMap r
map f                   = liftDIM $ IM.map f

filter                  :: (v -> Bool) -> DocIdMap v -> DocIdMap v
filter p                = liftDIM $ IM.filter p

filterWithKey           :: (DocId -> v -> Bool) -> DocIdMap v -> DocIdMap v
filterWithKey p         = liftDIM $ IM.filterWithKey p

mapWithKey              :: (DocId -> v -> r) -> DocIdMap v -> DocIdMap r
mapWithKey f            = liftDIM $ IM.mapWithKey f

traverseWithKey         :: Applicative t => (DocId -> a -> t b) -> DocIdMap a -> t (DocIdMap b)
traverseWithKey f       = (pure DIM <*>) . IM.traverseWithKey f . unDIM

foldr                   :: (v -> b -> b) -> b -> DocIdMap v -> b
foldr f u               = IM.foldr f u . unDIM

foldrWithKey            :: (DocId -> v -> b -> b) -> b -> DocIdMap v -> b
foldrWithKey f u        = IM.foldrWithKey f u . unDIM

fromList                :: [(DocId, v)] -> DocIdMap v
fromList                = DIM . IM.fromList

fromAscList             :: [(DocId, v)] -> DocIdMap v
fromAscList             = DIM . IM.fromAscList

toList                  :: DocIdMap v -> [(DocId, v)]
toList                  = IM.toList . unDIM

keys                    :: DocIdMap v -> [DocId]
keys                    = IM.keys . unDIM

elems                   :: DocIdMap v -> [v]
elems                   = IM.elems . unDIM

instance Binary v => Binary (DocIdMap v) where
    put                 = B.put . toList
    get                 = B.get >>= return . fromList

-- ------------------------------------------------------------

{-# INLINE liftDIM #-}
{-# INLINE liftDIM2 #-}
{-# INLINE empty #-}
{-# INLINE singleton #-}
{-# INLINE null #-}
{-# INLINE member #-}
{-# INLINE lookup #-}
{-# INLINE insert #-}
{-# INLINE delete #-}
{-# INLINE insertWith #-}
{-# INLINE size #-}
{-# INLINE minKey #-}
{-# INLINE maxKey #-}
{-# INLINE isIntervall #-}
{-# INLINE union #-}
{-# INLINE difference #-}
{-# INLINE unionWith #-}
{-# INLINE intersectionWith #-}
{-# INLINE differenceWith #-}
{-# INLINE unionsWith #-}
{-# INLINE map #-}
{-# INLINE filter #-}
{-# INLINE filterWithKey #-}
{-# INLINE mapWithKey #-}
{-# INLINE foldr #-}
{-# INLINE foldrWithKey #-}
{-# INLINE fromList #-}
{-# INLINE toList #-}
{-# INLINE keys #-}
{-# INLINE elems #-}

-- ------------------------------------------------------------
