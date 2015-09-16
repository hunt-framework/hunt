{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.Common.SegmentMap where

import           Control.Arrow
import           Control.DeepSeq
import           Data.Binary
import           Data.IntMap.BinTree.Strict (IntMap)
import qualified Data.IntMap.BinTree.Strict as IntMap
import           Data.Monoid

newtype SegmentId
  = SegmentId { unSegmentId :: Int }
    deriving (Binary, Enum, Eq, Ord, NFData)

instance Show SegmentId where
  show = show . unSegmentId

newtype SegmentMap a
  = SegmentMap { unSegmentMap :: IntMap a }
    deriving (Binary, Functor, Foldable, Traversable, NFData)

instance Monoid a => Monoid (SegmentMap a) where
  mempty = SegmentMap IntMap.empty
  mappend = unionWith (<>)

liftSM :: (IntMap a -> IntMap b) -> SegmentMap a -> SegmentMap b
liftSM f
  = SegmentMap . f . unSegmentMap

liftSM2 :: (IntMap a -> IntMap b -> IntMap c)
        -> SegmentMap a
        -> SegmentMap b
        -> SegmentMap c
liftSM2 f x y
  = SegmentMap (f (unSegmentMap x) (unSegmentMap y))

empty :: SegmentMap a
empty = SegmentMap IntMap.empty

size :: SegmentMap a -> Int
size = IntMap.size . unSegmentMap

elems :: SegmentMap a -> [a]
elems = IntMap.elems . unSegmentMap

keys :: SegmentMap a -> [SegmentId]
keys = fmap SegmentId . IntMap.keys . unSegmentMap

insert :: SegmentId -> a -> SegmentMap a -> SegmentMap a
insert (SegmentId k) v = liftSM (IntMap.insert k v)

insertWith :: (a -> a -> a) -> SegmentId -> a -> SegmentMap a -> SegmentMap a
insertWith f (SegmentId k) v = liftSM (IntMap.insertWith f k v)

delete :: SegmentId -> SegmentMap a -> SegmentMap a
delete (SegmentId k) = liftSM (IntMap.delete k)

unionWith :: (a -> a -> a) -> SegmentMap a -> SegmentMap a -> SegmentMap a
unionWith f = liftSM2 (IntMap.unionWith f)

intersectionWith :: (a -> b -> c) -> SegmentMap a -> SegmentMap b -> SegmentMap c
intersectionWith f = liftSM2 (IntMap.intersectionWith f)

difference :: SegmentMap a -> SegmentMap b -> SegmentMap a
difference = liftSM2 IntMap.difference

differenceWith :: (a -> b -> Maybe a) -> SegmentMap a -> SegmentMap b -> SegmentMap a
differenceWith f = liftSM2 (IntMap.differenceWith f)

fromList :: [(SegmentId, a)] -> SegmentMap a
fromList
  = SegmentMap . IntMap.fromList . fmap (first unSegmentId)

toList :: SegmentMap a -> [(SegmentId, a)]
toList = fmap (first SegmentId) . IntMap.toList . unSegmentMap

{-# INLINE liftSM #-}
{-# INLINE liftSM2 #-}
{-# INLINE empty #-}
{-# INLINE insert #-}
{-# INLINE delete #-}
{-# INLINE size #-}
{-# INLINE difference #-}
{-# INLINE unionWith #-}
{-# INLINE intersectionWith #-}
{-# INLINE differenceWith #-}
{-# INLINE fromList #-}
{-# INLINE toList #-}
{-# INLINE keys #-}
{-# INLINE elems #-}
