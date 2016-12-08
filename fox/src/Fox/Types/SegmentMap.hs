{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Fox.Types.SegmentMap (
    module Fox.Types.SegmentMap
  , index
  , lookupDefault
  , adjust
  , replace

  , forWithKey
  , forWithKey_
  , forWithKeyM
  , forWithKeyM_
  ) where

import           Fox.Types.SegmentId

import           Control.Arrow       (first)
import           Data.Binary         (Binary)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Data.Key

type SegmentSet = SegmentMap ()

-- | A map indexed by 'SegmentId'.
newtype SegmentMap a = SM { unSM :: IntMap a }
                     deriving ( Functor, Keyed, Zip, ZipWithKey
                              , Lookup, Indexable, Adjustable
                              , FoldableWithKey, Foldable
                              , Monoid
                              , Traversable
                              , Show
                              , Binary
                              )

type instance Key SegmentMap = SegmentId

instance TraversableWithKey SegmentMap where
  traverseWithKey f (SM im) =
    SM <$> IM.traverseWithKey (\k v -> f (SegmentId k) v) im

null :: SegmentMap a -> Bool
null = IM.null . unSM

size :: SegmentMap a -> Int
size = IM.size . unSM

empty :: SegmentMap a
empty = SM IM.empty

insert :: SegmentId -> a -> SegmentMap a -> SegmentMap a
insert (SegmentId k) v (SM m) = SM (IM.insert k v m)

map :: (a -> b) -> SegmentMap a -> SegmentMap b
map f (SM m) = SM (IM.map f m)

filter :: (a -> Bool) -> SegmentMap a -> SegmentMap a
filter p (SM m) = SM (IM.filter p m)

union :: SegmentMap a -> SegmentMap a -> SegmentMap a
union (SM m1) (SM m2) = SM (IM.union m1 m2)

unionWith :: (a -> a -> a) -> SegmentMap a -> SegmentMap a -> SegmentMap a
unionWith f (SM m1) (SM m2) = SM (IM.unionWith f m1 m2)

intersection :: SegmentMap a -> SegmentMap b -> SegmentMap a
intersection (SM m1) (SM m2) = SM (IM.intersection m1 m2)

intersectionWith :: (a -> b -> c)
                 -> SegmentMap a
                 -> SegmentMap b
                 -> SegmentMap c
intersectionWith f (SM m1) (SM m2) = SM (IM.intersectionWith f m1 m2)

intersectionWithKey :: (SegmentId -> a -> b -> c)
                    -> SegmentMap a
                    -> SegmentMap b
                    -> SegmentMap c
intersectionWithKey f (SM m1) (SM m2) =
  SM (IM.intersectionWithKey (\k x y -> f (SegmentId k) x y) m1 m2)

difference :: SegmentMap a -> SegmentMap b -> SegmentMap a
difference (SM m1) (SM m2) = SM (IM.difference m1 m2)

differenceWith :: (a -> b -> Maybe a)
               -> SegmentMap a
               -> SegmentMap b
               -> SegmentMap a
differenceWith f (SM m1) (SM m2) = SM (IM.differenceWith f m1 m2)

fromList :: [(SegmentId, a)] -> SegmentMap a
fromList xs = SM (IM.fromList (fmap (first unSegmentId) xs))

elems :: SegmentMap a -> [a]
elems (SM m) = IM.elems m

findMax :: SegmentMap a -> Maybe (SegmentId, a)
findMax (SM m)
  | IM.null m = Nothing
  | otherwise = case IM.findMax m of
                  (k, v) -> Just (SegmentId k, v)
