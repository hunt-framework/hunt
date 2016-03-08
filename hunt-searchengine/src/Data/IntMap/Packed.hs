{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.IntMap.Packed where

import           Control.Applicative hiding (empty)
import           Control.Arrow
import           Control.DeepSeq
import           Data.Binary
import           Data.Bits
import           Data.Maybe
import qualified Data.Traversable (Traversable(traverse))
import           Data.Foldable hiding (null, toList)
import qualified Data.Foldable as F
import           Data.Typeable
import qualified Data.Vector as Vector
import qualified Data.Vector as MVector
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Fusion.Stream.Size as Stream
import qualified Data.Vector.Fusion.Util as Stream
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Hybrid as HVector
import qualified Data.Vector.Hybrid.Internal as HVector
import qualified Data.Vector.Hybrid.Mutable as HMVector
import qualified Data.Vector.Primitive as UVector
import qualified Data.Vector.Primitive.Mutable as UMVector
import qualified Data.IntSet.Packed as IntSet
import           Prelude hiding (lookup, null)
import Control.Monad.ST

type IV a = HVector.Vector UVector.Vector Vector.Vector (Int, a)
type IVM s a = HMVector.MVector UMVector.MVector MVector.MVector s (Int, a)

newtype IntMap a
  = IntMap { unIntMap :: HVector.Vector UVector.Vector Vector.Vector (Int, a) }
  deriving (Eq, Show, Typeable)

instance NFData a => NFData (IntMap a) where
  rnf (IntMap (HVector.V _ vals))
    = rnf vals `seq` ()

instance Binary a => Binary (IntMap a) where
  put (IntMap v)
    = do put (HVector.length v)
         HVector.forM_ v $ \x -> put x >> return x
  get
    = do n <- get
         v <- HVector.replicateM n get
         return (IntMap v)

instance Functor IntMap where
  fmap f (IntMap (HVector.V keys vals))
    = IntMap (HVector.V keys (fmap f vals))
  {-# INLINE fmap #-}

instance Foldable IntMap where
  fold = fold . values
  {-# INLINE fold#-}

  foldMap f = foldMap f . values
  {-# INLINE foldMap#-}

  foldr f e = foldr f e . values
  {-# INLINE foldr #-}

  foldr' f e = foldr' f e . values
  {-# INLINE foldr' #-}

  foldl f e = foldl f e . values
  {-# INLINE foldl #-}

  foldl' f e = foldl' f e . values
  {-# INLINE foldl' #-}

  foldr1 f = foldr1 f . values
  {-# INLINE foldr1 #-}

  foldl1 f = foldl1 f . values
  {-# INLINE foldl1 #-}

  toList = F.toList . values
  {-# INLINE toList #-}

  null = Data.IntMap.Packed.null
  {-# INLINE null #-}

  length = Data.IntMap.Packed.size
  {-# INLINE length #-}

  elem e = elem e . values
  {-# INLINE elem #-}

  maximum = maximum . values
  {-# INLINE maximum #-}

  minimum = minimum . values
  {-# INLINE minimum #-}

  sum = sum . values
  {-# INLINE sum #-}

  product = product . values
  {-# INLINE product #-}

empty :: IntMap a
empty
  = IntMap HVector.empty
{-# INLINE empty #-}

null :: IntMap a -> Bool
null (IntMap v)
  = HVector.null v
{-# INLINE null #-}

size :: IntMap a -> Int
size (IntMap v)
  = HVector.length v

singleton :: Int -> a -> IntMap a
singleton k v
  = IntMap (HVector.singleton (k, v))
{-# INLINE singleton #-}

lookup :: Int -> IntMap a -> Maybe a
lookup x (IntMap (HVector.V keys vals))
  = loop 0 (UVector.length keys)
  where
    loop !l !u
      | u <= l    = Nothing
      | otherwise =
          case compare (keys `UVector.unsafeIndex` k) x of
           LT -> loop (k + 1) u
           EQ -> Just (Vector.unsafeIndex vals k)
           GT -> loop l k
      where
        i = u - l
        k = l + unsafeShiftR i 1 + unsafeShiftR i 6
{-# INLINE lookup #-}

findWithDefault :: Int -> a -> IntMap a -> a
findWithDefault k def
  = fromMaybe def . lookup k
{-# INLINE findWithDefault #-}

member :: Int -> IntMap a -> Bool
member k
  = maybe False (const True) . lookup k
{-# INLINE member #-}

notMember :: Int -> IntMap a -> Bool
notMember k
  = not . member k
{-# INLINE notMember #-}

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f im1@(IntMap v1) im2@(IntMap v2)
  | null im1  = im2
  | null im2  = im1
  | otherwise =
      IntMap (HVector.create (HMVector.new hint >>= union' f v1 v2))
  where
    hint = HVector.length v1 + HVector.length v2
{-# INLINE unionWith #-}

intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f im1@(IntMap v1) im2@(IntMap v2)
  | null im1  = empty
  | null im2  = empty
  | otherwise =
      IntMap (GVector.unstream (intersectStream f (GVector.stream v1) (GVector.stream v2)))
{-# INLINE intersectionWith #-}

intersectionWithSet :: IntMap a -> IntSet.IntSet -> IntMap a
intersectionWithSet im1@(IntMap v1) is1@(IntSet.DIS1 v2)
  | null im1 = empty
  | IntSet.null is1 = empty
  | otherwise =
      IntMap (GVector.unstream (intersectStream (\a _ -> a)
                                (GVector.stream v1)
                                (Stream.map (\x -> (x, x)) (GVector.stream v2))
                               ))
{-# INLINE intersectionWithSet #-}

differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f im1@(IntMap v1) im2@(IntMap v2)
  | null im2 = im1
  | null im1 = empty
  | otherwise =
      IntMap (GVector.unstream (differenceStream f (GVector.stream v1) (GVector.stream v2)))
{-# INLINE differenceWith #-}

differenceWithSet :: IntMap a -> IntSet.IntSet -> IntMap a
differenceWithSet im1@(IntMap v1) is1@(IntSet.DIS1 v2)
  | IntSet.null is1 = im1
  | null im1        = empty
  | otherwise =
      IntMap (GVector.unstream (differenceStream (\_ _ -> Nothing)
                                (GVector.stream v1)
                                (Stream.map (\x -> (x, x)) (GVector.stream v2))
                               ))
{-# INLINE differenceWithSet #-}

union :: IntMap a -> IntMap a -> IntMap a
union
  = unionWith const
{-# INLINE union #-}

unionsWith :: (a -> a -> a) -> [IntMap a] -> IntMap a
unionsWith f
  = Data.Foldable.foldr (unionWith f) empty

intersection :: IntMap a -> IntMap b -> IntMap a
intersection
  = intersectionWith const
{-# INLINE intersection #-}

difference :: IntMap a -> IntMap b -> IntMap a
difference
  = differenceWith (const (const Nothing))
{-# INLINE difference #-}

keys :: IntMap a -> [Int]
keys (IntMap (HVector.V keys _))
  = UVector.toList keys
{-# INLINE keys #-}

elems :: IntMap a -> [a]
elems (IntMap (HVector.V _ vals))
  = Vector.toList vals
{-# INLINE elems #-}

toList :: IntMap a -> [(Int, a)]
toList (IntMap v)
  = HVector.toList v
{-# INLINE toList #-}

fromAscList :: [(Int, a)] -> IntMap a
fromAscList
  = IntMap . HVector.fromList
{-# INLINE fromAscList #-}

fromList :: [(Int, a)] -> IntMap a
fromList
  = fromAscList
{-# INLINE fromList #-}

fromAscVec :: GVector.Vector v Int
           => v Int
           -> a
           -> IntMap a
fromAscVec v def
  = IntMap (HVector.V keys vals)
  where
    keys = GVector.convert v
    vals = Vector.replicate (GVector.length v) def
{-# INLINE fromAscVec #-}

fromIntSet :: (Int -> a) -> IntSet.IntSet -> IntMap a
fromIntSet f is@(IntSet.DIS1 v) =
  IntMap (HVector.V v (Vector.generate (IntSet.size is) f))
{-# INLINE fromIntSet #-}

keySet :: IntMap a -> IntSet.IntSet
keySet (IntMap (HVector.V keys _)) = IntSet.DIS1 keys
{-# INLINE keySet #-}

filterWithKey :: (Int -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey f
  = IntMap
    . GVector.unstream
    . Stream.filter (uncurry f)
    . GVector.stream
    . unIntMap
{-# INLINE filterWithKey #-}

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter f
  = filterWithKey (const f)
{-# INLINE filter #-}

foldrWithKey :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f s0
  = Stream.unId
    . Stream.foldr (\(k, v) b -> f k v b) s0
    . GVector.stream
    . unIntMap
{-# INLINE foldrWithKey #-}

foldlWithKey :: (b -> Int -> a -> b) -> b -> IntMap a -> b
foldlWithKey f s0
  = Stream.unId
    . Stream.foldl (\b (k, v) -> f b k v) s0
    . GVector.stream
    . unIntMap
{-# INLINE foldlWithKey #-}

values :: IntMap a -> Vector.Vector a
values (IntMap (HVector.V _ vals)) = vals
{-# INLINE values #-}

minViewWithKey :: IntMap a -> Maybe ((Int, a), IntMap a)
minViewWithKey (IntMap v)
  | HVector.null v = Nothing
  | otherwise      = Just ((k, a), IntMap rest)
      where
        (k, a) = HVector.head v
        rest   = HVector.tail v
{-# INLINE minViewWithKey #-}

minView :: IntMap a -> Maybe (a, IntMap a)
minView
  = fmap (first snd) . minViewWithKey
{-# INLINE minView #-}

maxViewWithKey :: IntMap a -> Maybe ((Int, a), IntMap a)
maxViewWithKey (IntMap v)
  | HVector.null v = Nothing
  | otherwise      = Just ((k, a), IntMap rest)
      where
        (k, a) = HVector.last v
        rest   = HVector.init v
{-# INLINE maxViewWithKey #-}

maxView :: IntMap a -> Maybe (a, IntMap a)
maxView
  = fmap (first snd) . minViewWithKey
{-# INLINE maxView #-}

insert :: Int -> a -> IntMap a -> IntMap a
insert k v im
  = union (singleton k v) im
{-# INLINE insert #-}

insertWith :: (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWith f k v im
  = unionWith f (singleton k v) im
{-# INLINE insertWith #-}

delete :: Int -> IntMap a -> IntMap a
delete k im
  = difference im (singleton k (0 :: Int))
{-# INLINE delete #-}

map :: (a -> b) -> IntMap a -> IntMap b
map = fmap
{-# INLINE map #-}

mapWithKey :: (Int -> a -> b) -> IntMap a -> IntMap b
mapWithKey f
  = IntMap
    . GVector.unstream
    . Stream.map (\(k, v) -> (k, f k v))
    . GVector.stream
    . unIntMap
{-# INLINE mapWithKey #-}

mapMWithKey :: Monad m
            => (Int -> a -> m b)
            -> IntMap a
            -> m (IntMap b)
mapMWithKey f (IntMap v)
  = do v' <-  (HVector.mapM (\(k, v) -> do b <- f k v
                                           return (k, b)
                            )) v
       return (IntMap v')
{-# INLINE mapMWithKey #-}

sizeWithLimit :: Int -> IntMap a -> Maybe Int
sizeWithLimit limit im
  = if sz > limit then Nothing else Just sz
  where
    sz = size im
{-# INLINE sizeWithLimit #-}

union' :: (a -> a -> a) -> IV a -> IV a -> IVM s a -> ST s (IVM s a)
union' f xs0 ys0 !out = do
  i <- go xs0 ys0 0
  return $! HMVector.take i out
  where
    go !xs !ys !i
      | HVector.null xs = do HVector.copy (HMVector.slice i (HVector.length ys) out) ys
                             return (i + HVector.length ys)
      | HVector.null ys = do HVector.copy (HMVector.slice i (HVector.length xs) out) xs
                             return (i + HVector.length xs)
      | otherwise = let (!x_k, x_v) = HVector.head xs
                        (!y_k, y_v) = HVector.head ys
                    in case compare x_k y_k of
                         GT -> do HMVector.write out i (y_k, y_v)
                                  go xs (HVector.tail ys) (i + 1)
                         EQ -> do HMVector.write out i (x_k, f x_v y_v)
                                  go (HVector.tail xs) (HVector.tail ys) (i + 1)
                         LT -> do HMVector.write out i (x_k, x_v)
                                  go (HVector.tail xs) ys (i + 1)

data U a b = U1 !a !b
           | U2 !a
           | U3 !b

unionStream :: Ord a
            => (b -> b -> b)
            -> Stream.Stream Stream.Id (a, b)
            -> Stream.Stream Stream.Id (a, b)
            -> Stream.Stream Stream.Id (a, b)
unionStream f (Stream.Stream next1 s1 n1) (Stream.Stream next2 s2 n2)
  = Stream.Stream next (U1 s1 s2) (Stream.toMax (n1 + n2))
  where
    {-# INLINE next #-}
    next (U1 s1 s2)
      = do r1 <- next1 s1
           case r1 of
             Stream.Yield (a, a') s1' -> do
               r2 <- next2 s2
               case r2 of
                 Stream.Yield (b, b') s2' -> do
                   case compare a b of
                     LT -> return $ Stream.Yield (a, a') (U1 s1' s2)
                     EQ -> return $ Stream.Yield (a, (f a' b')) (U1 s1' s2')
                     GT -> return $ Stream.Yield (b, b') (U1 s1  s2')
                 Stream.Skip s2'    -> return $ Stream.Skip (U1 s1 s2')
                 Stream.Done        -> return $ Stream.Yield (a, a') (U2 s1')
             Stream.Skip s1'    -> return $ Stream.Skip (U1 s1'  s2)
             Stream.Done        -> return $ Stream.Skip (U3 s2)
    next (U2 s1)
      = do r <- next1 s1
           case r of
             Stream.Yield x s1' -> return $ Stream.Yield x (U2 s1')
             Stream.Skip s1'    -> return $ Stream.Skip (U2 s1')
             Stream.Done        -> return $ Stream.Done
    next (U3 s2)
      = do r <- next2 s2
           case r of
             Stream.Yield x s2' -> return $ Stream.Yield x (U3 s2')
             Stream.Skip s2'    -> return $ Stream.Skip (U3 s2')
             Stream.Done        -> return $ Stream.Done
{-# INLINE unionStream #-}

data I a b = I1 !a !b

intersectStream :: Ord a
                => (b -> c -> d)
                -> Stream.Stream Stream.Id (a, b)
                -> Stream.Stream Stream.Id (a, c)
                -> Stream.Stream Stream.Id (a, d)
intersectStream f (Stream.Stream next1 s1 n1) (Stream.Stream next2 s2 n2)
  = Stream.Stream next (I1 s1 s2) (Stream.toMax (Stream.smaller n1 n2))
  where
    {-# INLINE next #-}
    next (I1 s1 s2)
      = do r1 <- next1 s1
           case r1 of
             Stream.Yield (a, a') s1' -> do
               r2 <- next2 s2
               case r2 of
                 Stream.Yield (b, b') s2' -> do
                   case compare a b of
                     EQ -> return $ Stream.Yield (a, f a' b') (I1 s1' s2')
                     LT -> return $ Stream.Skip (I1 s1' s2)
                     GT -> return $ Stream.Skip (I1 s1 s2')
                 Stream.Skip s2'    -> return $ Stream.Skip (I1 s1 s2')
                 Stream.Done        -> return Stream.Done
             Stream.Skip s1'    -> return $ Stream.Skip (I1 s1' s2)
             Stream.Done        -> return $ Stream.Done
{-# INLINE intersectStream #-}

data D a b = D1 !a !b
           | D2 !a

differenceStream :: Ord a
                => (b -> c -> Maybe b)
                -> Stream.Stream Stream.Id (a, b)
                -> Stream.Stream Stream.Id (a, c)
                -> Stream.Stream Stream.Id (a, b)
differenceStream f (Stream.Stream next1 s1 n1) (Stream.Stream next2 s2 n2)
  = Stream.Stream next (D1 s1 s2) (Stream.toMax n1)
  where
    {-# INLINE next #-}
    next (D1 s1 s2)
      = do r1 <- next1 s1
           case r1 of
             Stream.Yield (a, a') s1' -> do
               r2 <- next2 s2
               case r2 of
                 Stream.Yield (b, b') s2' -> do
                   case compare a b of
                     EQ -> case f a' b' of
                            Nothing  -> return $ Stream.Skip (D1 s1' s2')
                            Just a'' -> return $ Stream.Yield (a, a'') (D1 s1' s2')
                     GT -> return $ Stream.Skip (D1 s1 s2')
                     LT -> return $ Stream.Yield (a, a') (D1 s1' s2)
                 Stream.Skip s2' -> return $ Stream.Skip (D1 s1 s2')
                 Stream.Done     -> return $ Stream.Yield (a, a') (D2 s1')
             Stream.Skip s1' -> return $ Stream.Skip (D2 s1')
             Stream.Done     -> return $ Stream.Done
    next (D2 s1)
      = do r <- next1 s1
           case r of
             Stream.Yield x s1' -> return $ Stream.Yield x (D2 s1')
             Stream.Skip s1'    -> return $ Stream.Skip (D2 s1')
             Stream.Done        -> return $ Stream.Done
{-# INLINE differenceStream #-}
