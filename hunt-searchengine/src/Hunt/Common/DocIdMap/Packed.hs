{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.Common.DocIdMap.Packed where

import           Hunt.Common.DocId
import           Hunt.Common.DocIdSet

import           Control.Arrow
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.IntMap.Packed   (IntMap)
import qualified Data.IntMap.Packed   as IntMap
import           Data.Monoid
import           Data.Typeable

newtype DocIdMap v =
  DIM { unDIM :: IntMap v }
  deriving (Binary, Eq, Show, Foldable, Functor, NFData, Typeable)

instance ToJSON v => ToJSON (DocIdMap v) where
  toJSON = object . fmap toJ . IntMap.toList . unDIM
    where
      toJ (k, v) = toHex k .= toJSON v

instance Monoid v => Monoid (DocIdMap v) where
  mempty = empty
  {-# INLINE mempty #-}

  mappend = unionWith (<>)
  {-# INLINE mappend #-}

liftDIM                 :: (IntMap v -> IntMap r) ->
                           DocIdMap v -> DocIdMap r
liftDIM f               = DIM . f . unDIM

liftDIM2                :: (IntMap v -> IntMap w -> IntMap x) ->
                           DocIdMap v -> DocIdMap w -> DocIdMap x
liftDIM2 f x y          = DIM $ f (unDIM x) (unDIM y)

empty :: DocIdMap v
empty = DIM IntMap.empty

singleton :: DocId -> v -> DocIdMap v
singleton d v = insert d v empty

null :: DocIdMap v -> Bool
null = IntMap.null . unDIM

member :: DocId -> DocIdMap v -> Bool
member x = IntMap.member (unDocId x) . unDIM

-- | Lookup the value at a 'DocId' in the map.

--   The function will return the corresponding value as @('Just' value)@,
--   or 'Nothing' if the 'DocId' isn't in the map.
lookup                  :: DocId -> DocIdMap v -> Maybe v
lookup x                = IntMap.lookup (unDocId x) . unDIM

insert :: DocId -> v -> DocIdMap v -> DocIdMap v
insert x y = liftDIM $ IntMap.insert (unDocId x) y

insertWith              :: (v -> v -> v) -> DocId -> v -> DocIdMap v -> DocIdMap v
insertWith f x y        = liftDIM $ IntMap.insertWith f (unDocId x) y

-- | The number of elements in the map.
size                    :: DocIdMap v -> Int
size                    = IntMap.size . unDIM

-- | The number of elements limited up to a maximum
sizeWithLimit           :: Int -> DocIdMap v -> Maybe Int
sizeWithLimit limit     = IntMap.sizeWithLimit limit . unDIM

-- | The (left-biased) union of two maps.
--   It prefers the first map when duplicate 'DocId' are encountered,
--   i.e. @(union == unionWith const)@.
union                   :: DocIdMap v -> DocIdMap v -> DocIdMap v
union                   = liftDIM2 $ IntMap.union

-- | The (left-biased) intersection of two maps (based on 'DocId's).
intersection            :: DocIdMap v -> DocIdMap v -> DocIdMap v
intersection            = liftDIM2 $ IntMap.intersection

intersectionWithSet     :: DocIdMap v -> DocIdSet -> DocIdMap v
intersectionWithSet (DIM m) (DIS s) = DIM (IntMap.intersectionWithSet m s)

-- | Difference between two maps (based on 'DocId's).
difference              :: DocIdMap v -> DocIdMap w -> DocIdMap v
difference              = liftDIM2 $ IntMap.difference

-- | Difference between the map and a set of 'DocId's.
diffWithSet             :: DocIdMap v -> DocIdSet -> DocIdMap v
diffWithSet (DIM m) (DIS s) = DIM (IntMap.differenceWithSet m s)

-- | The union with a combining function.
unionWith               :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
unionWith f             = liftDIM2 $ IntMap.unionWith f

-- | The intersection with a combining function.
intersectionWith        :: (v1 -> v2 -> v3) -> DocIdMap v1 -> DocIdMap v2 -> DocIdMap v3
intersectionWith f      = liftDIM2 $ IntMap.intersectionWith f

-- | Difference with a combining function.
differenceWith          :: (v -> v -> Maybe v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
differenceWith f        = liftDIM2 $ IntMap.differenceWith f

-- | The union of a list of maps, with a combining operation.
unionsWith              :: (v -> v -> v) -> [DocIdMap v] -> DocIdMap v
unionsWith f            = DIM . IntMap.unionsWith f . fmap unDIM

-- | Map a function over all values in the map.
map                     :: (v -> r) -> DocIdMap v -> DocIdMap r
map f                   = liftDIM $ IntMap.map f

-- | Fold the values in the map using the given right-associative binary operator, such that
--   @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
--foldr                   :: (v -> b -> b) -> b -> DocIdMap v -> b
--foldr f u               = foldr f u . unDIM

--foldl'                   :: (b -> v -> b) -> b -> DocIdMap v -> b
--foldl' f u               = foldl' f u . unDIM

fromAscList :: [(DocId, v)] -> DocIdMap v
fromAscList = DIM . IntMap.fromAscList . fmap (first unDocId)

fromDocIdSet :: (DocId -> a) -> DocIdSet -> DocIdMap a
fromDocIdSet f (DIS is) =
  DIM (IntMap.fromIntSet (f . DocId) is)

keys :: DocIdMap a -> DocIdSet
keys (DIM im) = DIS (IntMap.keySet im)

toList :: DocIdMap a -> [(DocId, a)]
toList = fmap (first DocId) . IntMap.toList . unDIM

{-# INLINE liftDIM #-}
{-# INLINE liftDIM2 #-}
{-# INLINE empty #-}
{-# INLINE singleton #-}
{-# INLINE null #-}
{-# INLINE member #-}
{-# INLINE lookup #-}
{-# INLINE insert #-}
{-# INLINE insertWith #-}
{-# INLINE size #-}
{-# INLINE union #-}
{-# INLINE difference #-}
{-# INLINE diffWithSet #-}
{-# INLINE unionWith #-}
{-# INLINE intersectionWith #-}
{-# INLINE differenceWith #-}
{-# INLINE unionsWith #-}
{-# INLINE map #-}
{-# INLINE fromAscList #-}
{-# INLINE fromDocIdSet #-}
{-# INLINE keys #-}
{-# INLINE toList #-}
