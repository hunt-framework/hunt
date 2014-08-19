-- ----------------------------------------------------------------------------

{- |
   Intermediate data structure for sorting and paging a result set

   When sorting a result set by priority and knowing how many
   results are requested and which "page" of the result set
   is requested, a priority queue with limited capacity can be used
   for efficient sorting and paging.

-}

module Data.LimitedPriorityQueue
    ( Queue
    , mkQueue
    , insert
    , reduce
    , toList
    , fromList
    , pageList
    )
where

import           Prelude hiding (drop, take)

-- ----------------------------------------------------------------------------

data Queue v
    = Q { _capacity :: !Int
        , _size     :: !Int
        , _elems    :: (Heap v)
        }
      deriving (Show)

data Heap v
    = E
    | T v (Heap v) (Heap v)
      deriving (Show)

-- | Create an empty priority queue with a limited capacity.
-- If capacity is < 0, no limit is set.

mkQueue :: Int -> Queue v
mkQueue c
    | c >= 0
        = Q c 0 E
    | otherwise
        = Q maxBound 0 E

-- | Insert an element if there is space in the queue
-- or if the element is larger than the smallest element.

insert :: Ord v => v -> Queue v -> Queue v
insert x q@(Q c s h)
    | s < c                     -- capacity not reached: insert
        = Q c (s + 1) (merge h (T x E E))
    | x' >= x                   -- queue full, score < min score: don't insert
        = q
    | otherwise                 -- queue full, score >= min: throw min away and insert
        = Q c s (merge (merge l r) (T x E E))
    where
      (T x' l r) = h

-- | Reduce size and capacity of queue
-- by throwing away small elements.

reduce :: Ord v => Int -> Queue v -> Queue v
reduce i (Q _ s h)
    | i >= s
        = Q i s h
    | otherwise -- i < s
        = Q i i (drop (s - i) h)

toList :: Ord v => Int -> Int -> Queue v -> [v]
toList start len (Q _ s h)
    | len < 0                   -- no limit on the length, take all except the first elems
        = take (s - start) h
    | len' < s                  -- more elements in queue than needed, drop some elems
        = take len $ drop (s - len') h
    | otherwise                 -- less elements in queue, take them
        = take (len - (len' - s)) h
      where
        len' = start + len

fromList :: Ord v => Int -> [v] -> Queue v
fromList c
    = foldl (\ q x -> insert x q) (mkQueue c)

-- | Take a list of scored values, sort it and return a page of the result.
--
-- @pageList 10 5 xs == take 5 . drop 10 . sortBy snd $ xs@
--
-- If the length is set to @-1@ no limit on the page length is set.

pageList :: Ord v => Int -> Int -> [v] -> [v]
pageList start len
    = toList start len . fromList len'
    where
      len'
          | len >= 0  = start + len
          | otherwise = len

-- ----------------------------------------------------------------------------
-- skew heap operations

take :: Ord v => Int -> Heap v -> [v]
take i0 h0
    = take' i0 h0 []
    where
      take' i (T x l r) acc
          | i <= 0
              = acc
          | otherwise
              = take' (i - 1) (merge l r) (x : acc)
      take' _ E acc
            = acc

drop :: Ord v => Int -> Heap v -> Heap v
drop i h
    | i <= 0
        = h
    | otherwise
        = drop (i - 1) (merge l r)
    where
      (T _x l r) = h

merge :: Ord v => Heap v -> Heap v -> Heap v
merge E q2 = q2
merge q1 E = q1
merge q1@(T x1 l1 r1) q2@(T x2 l2 r2)
    | x1 <= x2
        = T x1 r1 (merge l1 q2)
    | otherwise
        = T x2 r2 (merge l2 q1)

-- ----------------------------------------------------------------------------

