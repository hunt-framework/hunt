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

module Holumbus.Index.Common.DocIdMap
    ( DocIdMap
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
    , unionWith
    , intersectionWith
    , differenceWith
    , unionsWith
    , map
    , filter
    , filterWithKey
    , mapWithKey
    , foldr
    , foldrWithKey
    , fromList
    , fromAscList
    , toList
    , keys
    , elems
    )
where

import           Control.DeepSeq
import           Prelude                     hiding (map, null, filter, lookup, foldr)
import qualified Prelude                     as P

import           Data.Binary                 (Binary (..))
import qualified Data.Binary                 as B
import qualified Data.EnumMap                as IM
import           Data.Foldable               hiding (toList, fold, foldr)

import           Holumbus.Index.Common.DocId


-- ------------------------------------------------------------

newtype DocIdMap v      = DIM { unDIM :: IM.EnumMap DocId v }
                          deriving (Eq, Show, Foldable, Functor)

liftDIM                 :: (IM.EnumMap DocId v -> IM.EnumMap DocId r) ->
                           DocIdMap v -> DocIdMap r
liftDIM f               = DIM . f . unDIM

liftDIM2                :: (IM.EnumMap DocId v -> IM.EnumMap DocId w -> IM.EnumMap DocId x) ->
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
minKey                  = maybe nullDocId (fst . fst) . IM.minViewWithKey . unDIM

maxKey                  :: DocIdMap v -> DocId
maxKey                  = maybe nullDocId (fst . fst) . IM.maxViewWithKey . unDIM

isIntervall             :: DocIdMap v -> Bool
isIntervall m           = null m
                                  ||
                                  ( fromEnum (theDocId (maxKey m)) - fromEnum (theDocId (minKey m))
                                    == size m - 1
                                  )

union                   :: DocIdMap v -> DocIdMap v -> DocIdMap v
union                   = liftDIM2 $ IM.union

intersection            :: DocIdMap v -> DocIdMap v -> DocIdMap v
intersection            = liftDIM2 $ IM.intersection

difference              :: DocIdMap v -> DocIdMap w -> DocIdMap v
difference              = liftDIM2 $ IM.difference

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

foldr                   :: (v -> b -> b) -> b -> DocIdMap v -> b
foldr f u               = IM.fold f u . unDIM -- XXX: replace with foldr?

foldrWithKey            :: (DocId -> v -> b -> b) -> b -> DocIdMap v -> b
foldrWithKey f u        = IM.foldWithKey f u . unDIM -- XXX: replace with foldrwithkey?

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

instance NFData v => NFData (DocIdMap v) where
    rnf                 = rnf . toList

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