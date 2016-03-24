{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
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
  , difference
  , empty
  , filter
  , foldr
  , fromAscList
  , fromList
  , intersection
  , intersectionWithDispl
  , member
  , minimum
  , notMember
  , null
  , singleton
  , size
  , split
  , split'
  , toAscList
  , toIntSet
  , toList
  , union
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
import qualified Data.Vector.Primitive.Mutable as MVector
import qualified Data.Vector.Generic as GVector

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
  | otherwise = DIS1 (Vector.create (MVector.new hint >>= union' s1 s2))
  where
    hint = Vector.length s1 + Vector.length s2
{-# INLINE union #-}

intersection :: IntSet -> IntSet -> IntSet
intersection i1@(DIS1 s1) i2@(DIS1 s2)
  | null i1 = empty
  | null i2 = empty
  | otherwise = DIS1 (Vector.create (MVector.new hint >>= intersect' id s1 s2))
  where
    hint = max (Vector.length s1) (Vector.length s2)
{-# INLINE intersection #-}

intersectionWithDispl :: Int -> IntSet -> IntSet -> IntSet
intersectionWithDispl !d i1@(DIS1 s1) i2@(DIS1 s2)
  | null i1 = empty
  | null i2 = empty
  | otherwise = DIS1 (Vector.create (MVector.new hint >>= intersect' (+d) s1 s2))
  where hint = max (Vector.length s1) (Vector.length s2)
{-# INLINE intersectionWithDispl #-}

difference :: IntSet -> IntSet -> IntSet
difference i1@(DIS1 s1) i2@(DIS1 s2)
  | null i2   = i1
  | otherwise = DIS1 (Vector.create (MVector.new hint >>= difference' s1 s2))
  where hint = Vector.length s1
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

union' :: Vector.Vector Int
       -> Vector.Vector Int
       -> MVector.MVector s Int
       -> ST s (MVector.MVector s Int)
union' xs0 ys0 !out = do
  i <- go xs0 ys0 0
  return $! MVector.take i out
  where
    go !xs !ys !i
      | Vector.null xs = do Vector.copy (MVector.slice i (Vector.length ys) out) ys
                            return (i + Vector.length ys)
      | Vector.null ys = do Vector.copy (MVector.slice i (Vector.length xs) out) xs
                            return (i + Vector.length xs)
      | otherwise = let x = Vector.head xs
                        y = Vector.head ys
                    in case compare x y of
                         GT -> do MVector.write out i y
                                  go xs (Vector.tail ys) (i + 1)
                         EQ -> do MVector.write out i x
                                  go (Vector.tail xs) (Vector.tail ys) (i + 1)
                         LT -> do MVector.write out i x
                                  go (Vector.tail xs) ys (i + 1)

difference' :: Vector.Vector Int
            -> Vector.Vector Int
            -> MVector.MVector s Int
            -> ST s (MVector.MVector s Int)
difference' xs0 ys0 !out = do
  i <- go xs0 ys0 0
  return $! MVector.take i out
  where
    go !xs !ys !i
      | Vector.null xs = return i
      | Vector.null ys = do Vector.copy (MVector.slice i (Vector.length xs) out) xs
                            return (i + Vector.length xs)
      | otherwise = let x = Vector.head xs
                        y = Vector.head ys
                     in case compare x y of
                          GT -> go xs (Vector.tail ys) i
                          EQ -> go (Vector.tail xs) (Vector.tail ys) i
                          LT -> do MVector.write out i x
                                   go (Vector.tail xs) ys (i + 1)

intersect' :: (Int -> Int)
           -> Vector.Vector Int
           -> Vector.Vector Int
           -> MVector.MVector s Int
           -> ST s (MVector.MVector s Int)
intersect' displ xs0 ys0 !out = do
  i <- go xs0 ys0 0
  return $! MVector.take i out
  where
    go !xs !ys !i
       | Vector.null xs = return i
       | Vector.null ys = return i
       | otherwise = let x = displ (Vector.head xs)
                         y = Vector.head ys
                     in case compare x y of
                          GT -> go xs (Vector.tail ys) i
                          EQ -> do MVector.write out i x
                                   go (Vector.tail xs) (Vector.tail ys) (i + 1)
                          LT -> go (Vector.tail xs) ys i
