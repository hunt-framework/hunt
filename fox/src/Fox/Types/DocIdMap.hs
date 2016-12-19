{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Fox.Types.DocIdMap
  ( DocIdMap(..)
  , empty
  , singleton
  , null
  , member
  , lookup
  , insert
  , delete
  , insertWith
  , size
  , sizeWithLimit
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
  , foldl
  , fromList
  , fromDocIdSet
  , fromAscList
  , toList
  , keys
  , elems
  )
where

import           Control.Arrow      (first)
import           Control.DeepSeq
import           Control.Monad      (foldM, mzero)
import           Data.Foldable      hiding (fold, foldl, foldr, null, toList)
import qualified Data.IntMap.Strict as IM
import qualified Data.Key           as K
import qualified Data.List          as L
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Typeable
import           Fox.Types.DocIdSet (DocIdSet (..), toIntSet)
import           Fox.Types.Document (DocId (..))
import           Prelude            hiding (filter, foldl, foldr, lookup, map,
                                     null)
import qualified Prelude            as P

-- ------------------------------------------------------------

-- | An efficient Map implementation for 'DocId's.

newtype DocIdMap v
  = DIM { unDIM :: IM.IntMap v }
  deriving (Eq, Show, Foldable, K.FoldableWithKey, {-Traversable,-} Functor, NFData, Typeable)

type instance K.Key DocIdMap = DocId

-- ------------------------------------------------------------

instance Monoid v => Monoid (DocIdMap v) where
    mempty  = DIM IM.empty
    mappend = unionWith (<>)

liftDIM                 :: (IM.IntMap v -> IM.IntMap r) ->
                           DocIdMap v -> DocIdMap r
liftDIM f               = DIM . f . unDIM

liftDIM2                :: (IM.IntMap v -> IM.IntMap w -> IM.IntMap x) ->
                           DocIdMap v -> DocIdMap w -> DocIdMap x
liftDIM2 f x y          = DIM $ f (unDIM x) (unDIM y)

-- | The empty map.
empty                   :: DocIdMap v
empty                   = DIM $ IM.empty

-- | A map with a single element.
singleton               :: DocId -> v -> DocIdMap v
singleton d v           = insert d v empty

-- | Is the map empty?
null                    :: DocIdMap v -> Bool
null                    = IM.null . unDIM

-- | Is the 'DocId' member of the map?
member                  :: DocId -> DocIdMap v -> Bool
member x                = IM.member (unDocId x) . unDIM

-- | Lookup the value at a 'DocId' in the map.

--   The function will return the corresponding value as @('Just' value)@,
--   or 'Nothing' if the 'DocId' isn't in the map.
lookup                  :: DocId -> DocIdMap v -> Maybe v
lookup x                = IM.lookup (unDocId x) . unDIM

-- | Insert a 'DocId' and value in the map.
--   If the 'DocId' is already present in the map, the associated value is replaced with the supplied
--   value. 'insert' is equivalent to 'insertWith' 'const'.
insert                  :: DocId -> v -> DocIdMap v -> DocIdMap v
insert x y              = liftDIM $ IM.insert (unDocId x) y

-- | Delete a 'DocId' and its value from the map.
--   When the 'DocId' is not a member of the map, the original map is returned.
delete                  :: DocId -> DocIdMap v -> DocIdMap v
delete x                = liftDIM $ IM.delete (unDocId x)

-- | Insert with a function, combining new value and old value.
--   @insertWith f docId value mp@ will insert the pair @(docId, value)@ into @mp@ if @docId@ does
--   not exist in the map. If the 'DocId' does exist, the function will insert the pair
--   @(docId, f new_value old_value)@.
insertWith              :: (v -> v -> v) -> DocId -> v -> DocIdMap v -> DocIdMap v
insertWith f x y        = liftDIM $ IM.insertWith f (unDocId x) y

-- | The number of elements in the map.
size                    :: DocIdMap v -> Int
size                    = IM.size . unDIM

-- | The number of elements limited up to a maximum
sizeWithLimit           :: Int -> DocIdMap v -> Maybe Int
sizeWithLimit _limit    = Just . IM.size . unDIM

-- | The (left-biased) union of two maps.
--   It prefers the first map when duplicate 'DocId' are encountered,
--   i.e. @(union == unionWith const)@.
union                   :: DocIdMap v -> DocIdMap v -> DocIdMap v
union                   = liftDIM2 $ IM.union

-- | The (left-biased) intersection of two maps (based on 'DocId's).
intersection            :: DocIdMap v -> DocIdMap v -> DocIdMap v
intersection            = liftDIM2 $ IM.intersection

-- | Difference between two maps (based on 'DocId's).
difference              :: DocIdMap v -> DocIdMap w -> DocIdMap v
difference              = liftDIM2 $ IM.difference

-- | Difference between the map and a set of 'DocId's.
diffWithSet             :: DocIdMap v -> DocIdSet -> DocIdMap v
diffWithSet m s         = m `difference` (DIM $ IM.fromSet (const ()) (unDIS s))

-- | The union with a combining function.
unionWith               :: (v -> v -> v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
unionWith f             = liftDIM2 $ IM.unionWith f

-- | The intersection with a combining function.
intersectionWith        :: (v1 -> v2 -> v3) -> DocIdMap v1 -> DocIdMap v2 -> DocIdMap v3
intersectionWith f      = liftDIM2 $ IM.intersectionWith f

-- | Difference with a combining function.
differenceWith          :: (v -> v -> Maybe v) -> DocIdMap v -> DocIdMap v -> DocIdMap v
differenceWith f        = liftDIM2 $ IM.differenceWith f

-- | The union of a list of maps, with a combining operation.
unionsWith              :: (v -> v -> v) -> [DocIdMap v] -> DocIdMap v
unionsWith f            = DIM . IM.unionsWith f . P.map unDIM

-- | Map a function over all values in the map.
map                     :: (v -> r) -> DocIdMap v -> DocIdMap r
map f                   = liftDIM $ IM.map f

-- | Map a function over all values in the map.
mapWithKey              :: (DocId -> v -> r) -> DocIdMap v -> DocIdMap r
mapWithKey f            = liftDIM $ IM.mapWithKey (f . DocId)

-- | Filter all values that satisfy some predicate.
filter                  :: (v -> Bool) -> DocIdMap v -> DocIdMap v
filter p                = liftDIM $ IM.filter p

-- | Filter all 'DocId's/values that satisfy some predicate.
filterWithKey           :: (DocId -> v -> Bool) -> DocIdMap v -> DocIdMap v
filterWithKey p         = liftDIM $ IM.filterWithKey (p . DocId)

-- | @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
--   That is, behaves exactly like a regular 'traverse' except that the traversing
--   function also has access to the 'DocId' associated with a value.
--
--   > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
--   > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey         :: Applicative t => (DocId -> a -> t b) -> DocIdMap a -> t (DocIdMap b)
traverseWithKey f       = (pure DIM <*>) . IM.traverseWithKey (f . DocId) . unDIM

-- | Fold the values in the map using the given right-associative binary operator, such that
--   @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr                   :: (v -> b -> b) -> b -> DocIdMap v -> b
foldr f u               = IM.foldr f u . unDIM

-- | Fold the 'DocId's and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey            :: (DocId -> v -> b -> b) -> b -> DocIdMap v -> b
foldrWithKey f u        = IM.foldrWithKey (f . DocId) u . unDIM

foldl                   :: (b -> v -> b) -> b -> DocIdMap v -> b
foldl f u               = IM.foldl f u . unDIM

-- | Create a map from a list of 'DocId'\/value pairs.
fromList                :: [(DocId, v)] -> DocIdMap v
fromList                = DIM . IM.fromList . L.map (first unDocId)

-- | Create a map from a set of 'DocId' values
fromDocIdSet            :: (Int -> v) -> DocIdSet -> DocIdMap v
fromDocIdSet f s        = DIM $ IM.fromSet f (toIntSet s)

-- | Build a map from a list of 'DocId'\/value pairs where the 'DocId's are in ascending order.
fromAscList             :: [(DocId, v)] -> DocIdMap v
fromAscList             = DIM . IM.fromAscList . L.map (first unDocId)

-- | Convert the map to a list of 'DocId'\/value pairs.
--   Subject to list fusion.
toList                  :: DocIdMap v -> [(DocId, v)]
toList                  = L.map (first DocId) . IM.toList . unDIM

-- | Return all 'DocId's of the map in ascending order.
--   Subject to list fusion.
keys                    :: DocIdMap v -> [DocId]
keys                    = L.map DocId . IM.keys . unDIM

-- | Return all elements of the map in the ascending order of their 'DocId's.
--   Subject to list fusion.
elems                   :: DocIdMap v -> [v]
elems                   = IM.elems . unDIM

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
