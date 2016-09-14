{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Hunt.SegmentIndex.Types.SegmentMap where

import           Hunt.SegmentIndex.Types.SegmentId

import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict                as IM
import           Data.Key

-- | A map indexed by 'SegmentId'.
newtype SegmentMap a = SM { unSM :: IntMap a }
                     deriving ( Functor, Keyed, Zip, ZipWithKey
                              , Lookup, Indexable, Adjustable
                              , FoldableWithKey, Foldable
                              , Traversable )

type instance Key SegmentMap = SegmentId

instance TraversableWithKey SegmentMap where
  traverseWithKey f (SM im) =
    SM <$> IM.traverseWithKey (\k v -> f (SegmentId k) v) im

null :: SegmentMap a -> Bool
null = IM.null . unSM

empty :: SegmentMap a
empty = SM IM.empty

insert :: SegmentId -> a -> SegmentMap a -> SegmentMap a
insert (SegmentId k) v (SM m) = SM (IM.insert k v m)
