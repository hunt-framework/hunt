{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.DocIdSet
  Copyright  : Copyright (C) 2014 Uwe Schmidt
  License    : MIT
  Maintainer : Uwe Schmidt

  Efficient Set implementation for 'DocId's.
-}

-- ----------------------------------------------------------------------------

module Data.IntSet.Packed
  ( IntSet(..)
  , empty
  , filter
  , singleton
  , null
  , size
  , member
  , notMember
  , foldr
  , minimum
  , split
  , split'
  , fromList
  , toIntSet
  , toList
  , toAscList
  , difference
  , union
  , fromAscList
  , intersection
  )
where

import           Prelude hiding (null, foldr, minimum, filter)

import           Control.DeepSeq
import           Control.Monad.ST

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.IntSet as S
import           Data.Typeable
import           Data.Binary (Binary (..))
import           Data.Bits

import qualified Data.Vector as VVector
import qualified Data.Vector.Algorithms.Intro as Sort
import qualified Data.Vector.Primitive as Vector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Fusion.Stream.Size as Stream
import qualified Data.Vector.Fusion.Util as Stream

-- ------------------------------------------------------------
--
-- the wrapped DocId set

newtype IntSet = DIS1 { unDIS1 :: Vector.Vector Int }
               deriving (Eq, Ord, Read, Show, NFData, Typeable)

instance Binary IntSet where
  put (DIS1 v) = do put (Vector.length v)
                    Vector.forM_ v put
  {-# INLINE put#-}

  get = do l <- get
           v <- Vector.replicateM l get
           return (DIS1 v)
  {-# INLINE get #-}

instance ToJSON IntSet where
  toJSON = toJSON . unDIS1

instance FromJSON IntSet where
  parseJSON x = fromVector <$> (parseJSON x :: Parser (VVector.Vector Int))

instance Monoid IntSet where
  mempty  = DIS1 Vector.empty
  {-# INLINE mempty #-}
  mappend = union
  {-# INLINE mappend #-}

empty :: IntSet
empty = DIS1 Vector.empty

singleton :: Int -> IntSet
singleton = DIS1 . Vector.singleton

null :: IntSet -> Bool
null = Vector.null . unDIS1

size :: IntSet -> Int
size = Vector.length . unDIS1

union :: IntSet -> IntSet -> IntSet
union i1@(DIS1 s1) i2@(DIS1 s2)
  | null i1 = i2
  | null i2 = i1
  | otherwise = DIS1 (GVector.unstream (unionStream (GVector.stream s1) (GVector.stream s2)))
{-# INLINE union #-}

intersection :: IntSet -> IntSet -> IntSet
intersection i1@(DIS1 s1) i2@(DIS1 s2)
  | null i1 = empty
  | null i2 = empty
  | otherwise = DIS1 (GVector.unstream (intersectStream (GVector.stream s1) (GVector.stream s2)))
{-# INLINE intersection #-}

difference :: IntSet -> IntSet -> IntSet
difference i1@(DIS1 s1) i2@(DIS1 s2)
  | null i2   = i1
  | otherwise = DIS1 (GVector.unstream (differenceStream (GVector.stream s1) (GVector.stream s2)))
{-# INLINE difference #-}

foldr :: (Int -> b -> b) -> b -> IntSet -> b
foldr f s0 (DIS1 v)
  = Vector.foldr f s0 v
{-# INLINE foldr #-}

filter :: (Int -> Bool) -> IntSet -> IntSet
filter p
  = DIS1 . Vector.filter p . unDIS1
{-# INLINE filter #-}

minimum :: IntSet -> Maybe Int
minimum (DIS1 v)
  = v Vector.!? 0
{-# INLINE minimum #-}

split' :: Int -> IntSet -> (Maybe Int, IntSet, IntSet)
split' n (DIS1 v) = loop 0 (Vector.length v)
  where
    splitVec i = Vector.splitAt i v
    loop !lb !ub
      | ub <= lb    = let (l, r) = splitVec lb  in (Nothing, DIS1 l, DIS1 r)
      | otherwise =
          case (compare (v `Vector.unsafeIndex` k) n) of
            LT -> loop (k + 1) ub
            EQ -> let
              (l, r) = splitVec k
              in (Just (Vector.head r), DIS1 l, DIS1 (Vector.tail r))
            GT -> loop lb k
      where
        k = (ub + lb) `unsafeShiftR` 1
{-# INLINE split' #-}

split :: Int -> IntSet -> (IntSet, IntSet)
split n (DIS1 v) = loop 0 (Vector.length v)
  where
    loop !lb !ub
      | ub <= lb  = let (l, r) = Vector.splitAt lb v in (DIS1 l, DIS1 r)
      | otherwise =
          case (compare (v `Vector.unsafeIndex` k) n) of
            LT -> loop (k + 1) ub
            EQ -> let (l, r) = Vector.splitAt lb v in (DIS1 l, DIS1 r)
            GT -> loop lb k
      where
        k = (ub + lb) `unsafeShiftR` 1
{-# INLINE split #-}

fromList :: [Int] -> IntSet
fromList xs = runST $ do
  v <- Vector.unsafeThaw (Vector.fromList xs)
  Sort.sort v
  DIS1 <$> Vector.unsafeFreeze v

fromAscList :: [Int] -> IntSet
fromAscList
  = DIS1 . Vector.fromList
{-# INLINE fromAscList #-}

fromVector :: GVector.Vector v Int => v Int -> IntSet
fromVector = DIS1 . GVector.convert
{-# INLINE fromVector #-}

toList :: IntSet -> [Int]
toList = Vector.toList . unDIS1
{-# INLINE toList #-}

toAscList :: IntSet -> [Int]
toAscList = toList
{-# INLINE toAscList #-}

member :: Int -> IntSet -> Bool
member x (DIS1 v)
  = loop 0 (Vector.length v)
  where
    loop !l !u
      | u <= l    = False
      | otherwise =
          case (compare (v `Vector.unsafeIndex` k) x) of
            LT -> loop (k + 1) u
            EQ -> True
            GT -> loop l k
      where
        k = (u + l) `unsafeShiftR` 1
{-# INLINE member #-}

notMember :: Int -> IntSet -> Bool
notMember x
  = not . member x
{-# INLINE notMember #-}

toIntSet :: IntSet -> S.IntSet
toIntSet = S.fromAscList . Vector.toList .  unDIS1
{-# INLINE toIntSet #-}

data D a b = D1 !a !b
           | D2 !a

differenceStream :: Ord a
                => Stream.Stream Stream.Id a
                -> Stream.Stream Stream.Id a
                -> Stream.Stream Stream.Id a
differenceStream (Stream.Stream next1 s n1) (Stream.Stream next2 s' _n2)
  = Stream.Stream next (D1 s s') (Stream.toMax n1)
  where
    {-# INLINE next #-}
    next (D1 s1 s2)
      = do r1 <- next1 s1
           case r1 of
             Stream.Yield x s1' -> do
               r2 <- next2 s2
               case r2 of
                 Stream.Yield y s2' -> do
                   case compare x y of
                     EQ -> return $ Stream.Skip (D1 s1' s2')
                     GT -> return $ Stream.Skip (D1 s1 s2')
                     LT -> return $ Stream.Yield x (D1 s1' s2)
                 Stream.Skip s2' -> return $ Stream.Skip (D1 s1 s2')
                 Stream.Done     -> return $ Stream.Yield x (D2 s1')
             Stream.Skip s1' -> return $ Stream.Skip (D2 s1')
             Stream.Done     -> return $ Stream.Done
    next (D2 s1)
      = do r <- next1 s1
           case r of
             Stream.Yield x s1' -> return $ Stream.Yield x (D2 s1')
             Stream.Skip s1'    -> return $ Stream.Skip (D2 s1')
             Stream.Done        -> return $ Stream.Done
{-# INLINE differenceStream #-}

data I a b = I1 !a !b

intersectStream :: Ord a
                => Stream.Stream Stream.Id a
                -> Stream.Stream Stream.Id a
                -> Stream.Stream Stream.Id a
intersectStream (Stream.Stream next1 s n1) (Stream.Stream next2 s' n2)
  = Stream.Stream next (I1 s s') (Stream.smaller n1 n2)
  where
    {-# INLINE next #-}
    next (I1 s1 s2)
      = do r1 <- next1 s1
           case r1 of
             Stream.Yield x s1' -> do
               r2 <- next2 s2
               case r2 of
                 Stream.Yield y s2' -> do
                   case compare x y of
                     EQ -> return $ Stream.Yield x (I1 s1' s2')
                     LT -> return $ Stream.Skip (I1 s1' s2)
                     GT -> return $ Stream.Skip (I1 s1 s2')
                 Stream.Skip s2'    -> return $ Stream.Skip (I1 s1 s2')
                 Stream.Done        -> return Stream.Done
             Stream.Skip s1'    -> return $ Stream.Skip (I1 s1' s2)
             Stream.Done        -> return $ Stream.Done
{-# INLINE intersectStream #-}

data U a b = U1 !a !b
           | U2 !a
           | U3 !b

unionStream :: Ord a
            => Stream.Stream Stream.Id a
            -> Stream.Stream Stream.Id a
            -> Stream.Stream Stream.Id a
unionStream (Stream.Stream next1 s n1) (Stream.Stream next2 s' n2)
  = Stream.Stream next (U1 s s') (Stream.toMax(n1 + n2))
  where
    {-# INLINE next #-}
    next (U1 s1 s2)
      = do r1 <- next1 s1
           case r1 of
             Stream.Yield x s1' -> do
               r2 <- next2 s2
               case r2 of
                 Stream.Yield y s2' -> do
                   case compare x y of
                     LT -> return $ Stream.Yield x (U1 s1' s2)
                     EQ -> return $ Stream.Yield x (U1 s1' s2')
                     GT -> return $ Stream.Yield y (U1 s1  s2')
                 Stream.Skip s2'    -> return $ Stream.Skip (U1 s1 s2')
                 Stream.Done        -> return $ Stream.Yield x (U2 s1')
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

-- ------------------------------------------------------------
