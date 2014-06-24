-- ----------------------------------------------------------------------------

{- |
   Intermediate data structure for sorting and paging a result set

   When sorting a result set by priority and knowing how many
   results are requested and which "page" of the result set
   is requested, a priority queue with limited capacity can be used
   for efficient sorting and paging.

-}

module Hunt.Common.LimitedPriorityQueue
    ( Queue
    , mkQueue
    , insert
    , reduce
    , toList
    , fromList
    , pageList
    )
where

import           Hunt.Common.BasicTypes (Score)
import           Prelude                hiding (drop, take)

-- ----------------------------------------------------------------------------

data Queue v
    = Q { _capacity :: !Int
        , _size     :: !Int
        , _elems    :: (Heap v)
        }
      deriving (Show)

data Heap v
    = E
    | T !Score v (Heap v) (Heap v)
      deriving (Show)

-- | create an empty priority queue with a limited capacity
-- if capacity is < 0, no limit is defined

mkQueue :: Int -> Queue v
mkQueue c
    | c >= 0
        = Q c 0 E
    | otherwise
        = Q maxBound 0 E

-- | insert an element if there's space in the queue
-- or if element is larger than smallest element

insert :: v -> Score -> Queue v -> Queue v
insert di sc q@(Q c s h)
    | s < c                     -- capacity not reached: insert
        = Q c (s + 1) (merge h (T sc di E E))
    | sc' >= sc                 -- queue full, score < min score: don't insert
        = q
    | otherwise                 -- queue full, score >= min: throw min away and insert
        = Q c s (merge (merge l r) (T sc di E E))
    where
      (T sc' _di' l r) = h

-- | reduce size and capacity of queue
-- by throwing away small elements

reduce :: Int -> Queue v -> Queue v
reduce i (Q _ s h)
    | i >= s
        = Q i s h
    | otherwise -- i < s
        = Q i i (drop (s - i) h)

toList :: Int -> Int -> Queue v -> [(v, Score)]
toList start len (Q _ s h)
    | len < 0                   -- no limit on the length, take all except the first elems
        = take (s - start) h
    | len' < s                  -- more elements in queue than needed, drop some elems
        = take len $ drop (s - len') h
    | otherwise                 -- less elements in queue, take them
        = take (len - (len' - s)) h
      where
        len' = start + len

fromList :: Int -> [(v, Score)] -> Queue v
fromList c
    = foldl (\ q (di, sc) -> insert di sc q) (mkQueue c)

-- | take a list of scored values, sort it and return a page of the result
--
-- @pageList 10 5 xs == take 5 . drop 10 . sortBy snd $ xs@
--
-- If the length is set to @-1@ no limit on the page length is set

pageList :: Int -> Int -> [(v, Score)] -> [(v, Score)]
pageList start len
    = toList start len . fromList len'
    where
      len'
          | len >= 0  = start + len
          | otherwise = len

-- ----------------------------------------------------------------------------
-- skew heap operations

take :: Int -> Heap v -> [(v, Score)]
take i0 h0
    = take' i0 h0 []
    where
      take' i (T sc di l r) acc
          | i <= 0
              = acc
          | otherwise
              = take' (i - 1) (merge l r) ((di, sc) : acc)
      take' _ E acc
            = acc

drop :: Int -> Heap v -> Heap v
drop i h
    | i <= 0
        = h
    | otherwise
        = drop (i - 1) (merge l r)
    where
      (T _sc _di l r) = h

merge :: Heap v -> Heap v -> Heap v
merge E q2 = q2
merge q1 E = q1
merge q1@(T sc1 di1 l1 r1) q2@(T sc2 di2 l2 r2)
    | sc1 <= sc2
        = T sc1 di1 r1 (merge l1 q2)
    | otherwise
        = T sc2 di2 r2 (merge l2 q1)

-- ----------------------------------------------------------------------------

