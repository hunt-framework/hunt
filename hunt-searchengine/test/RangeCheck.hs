module RangeCheck where

intToKey :: Int -> Int -> Int -> String
intToKey base len val = tok len val ""
    where
      tok 0 _ acc = acc
      tok i v acc = tok (i - 1) v' (d : acc)
          where
            (v', r) = v `divMod` base
            d       = toEnum (r + fromEnum '0')

intPairToKey :: Int -> Int -> (Int, Int) -> String
intPairToKey base len (x, y) = merge x' y'
    where
      x' = intToKey base len x
      y' = intToKey base len y

merge :: [a] -> [a] -> [a]
merge []       []       = []
merge (x : xs) (y : ys) = x : y : merge xs ys

intFromKey :: String -> Int
intFromKey = read

unMerge :: [a] -> ([a], [a])
unMerge [] = ([], [])
unMerge (x : y : s) = (x : xs, y : ys)
    where
      (xs, ys) = unMerge s


-- the real point

type Point = (Int, Int)

le :: Point -> Point -> Bool
le (x1, y1) (x2, y2)
    = x1 <= x2 && y1 <= y2

-- the point as string

newtype Point' = P' String
    deriving (Eq)

instance Show Point' where
    show (P' ds) = ds

instance Ord Point' where
    (P' s1) <= (P' s2) = s1 `le` s2
        where
          le [] [] = True
          le (x1 : y1 : ds1) (x2 : y2 : ds2)
              | x1 == x2 && y1 == y2 = ds1 `le`  ds2
              | x1 == x2 && y1 <  y2 = ds1 `leX` ds2
              | x1 <  x2 && y1 == y2 = ds1 `leY` ds2
              | x1 <  x2 && y1 <  y2 = True
              | otherwise            = False

          leX [] [] = True			-- the result for the Y dimension is already known
          leX (x1 : y1 : ds1) (x2 : y2 : ds2)
              | x1 == x2  = ds1 `leX` ds2
              | x1 <  x2  = True
              | otherwise = False

          leY [] [] = True			-- the result for the X dimension is already known
          leY (x1 : y1 : ds1) (x2 : y2 : ds2)
              | y1 == y2  = ds1 `leY` ds2
              | y1 <  y2  = True
              | otherwise = False

-- toPoint' and fromPoint': the bijection Point <-> Point'

toPoint' :: Point -> Point'
toPoint' p = P' $ intPairToKey base len p
    where
      base =  2		-- or 10
      len  =  10        -- or  3  (or something else)

fromPoint' :: Point' -> Point
fromPoint' (P' ds) = (intFromKey xs, intFromKey ys)
    where
      (xs, ys) = unMerge ds

-- the test, whether the `le` ordering is preserved, when working with Point'
propOrdered :: Point -> Point -> Bool
propOrdered p1 p2
    = (p1 `le` p2) == (toPoint' p1 <= toPoint' p2)

-- very quick check test
propTest :: Int -> [(Point, Point)]
propTest n
    = filter (not . uncurry propOrdered) qs
      where
        xs = [1..n]
        ps = [(x, y) | x <- xs, y <- xs]
        qs = [(p1, p2) | p1 <- ps, p2 <- ps]

test :: Bool
test = null $ propTest 20
