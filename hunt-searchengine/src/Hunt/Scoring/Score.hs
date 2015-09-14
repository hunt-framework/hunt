{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- ----------------------------------------------------------------------------

{- |
  Basic type for scoring an class ScoredResult for aggregating scores
-}

-- ----------------------------------------------------------------------------

module Hunt.Scoring.Score
where

import           Control.Applicative
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary         hiding (Word)
import           Data.Monoid

import           Prelude             as P

-- ------------------------------------------------------------

-- | Weight or score of a documents,
-- @0.0@ indicates: not set, so there is no need to work with Maybe's
--  wrapped in newtype to not mix up with Score's and Weight's in documents

newtype Score = SC {unScore :: Float}
    deriving (Eq, Ord, Num, Fractional, Show)

instance NFData Score where
  rnf (SC f) = f `seq` ()

noScore :: Score
noScore = SC 0.0

mkScore :: Float -> Score
mkScore x
    | x > 0.0   = SC x
    | otherwise = noScore

getScore :: Score -> Maybe Float
getScore (SC 0.0) = Nothing
getScore (SC x  ) = Just x

defScore :: Score
defScore = SC 1.0

toDefScore :: Score -> Score
toDefScore (SC 0.0) = defScore
toDefScore sc       = sc

fromDefScore :: Score -> Score
fromDefScore (SC 1.0) = noScore
fromDefScore sc       = sc

accScore :: [Score] -> Score
accScore [] = defScore
accScore xs = mkScore $ sum (P.map unScore xs) / fromIntegral (P.length xs)

-- ------------------------------------------------------------

-- the Monoid instance is used to accumulate scores
-- in query results, so tune it here when sum is not appropriate

instance Monoid Score where
    mempty  = noScore
    x `mappend` y
      | x == noScore = y
      | y == noScore = x
      | otherwise    = x `max` y

instance ScoredResult Score where
    boost             = (*)
    nullSC x          = x == noScore
    differenceSC x _y = x
    intersectSC       = (+) -- or  (*) ???

instance FromJSON Score where
    parseJSON x = mkScore <$> parseJSON x

instance ToJSON Score where
    toJSON (SC x) = toJSON x

instance Binary Score where
    put (SC x) = put x
    get = SC <$> get

-- ------------------------------------------------------------
--
-- The Monoid @mempty@ acts as empty result,
-- (<>) is the union of scored results

-- | Common ops for scored results
--
-- union of results is implemented by the Monoid instance

class Monoid a => ScoredResult a where
  -- | boost a result by a factor
  boost            :: Score -> a -> a

  -- | check, whether result is empty
  nullSC           :: a -> Bool

  -- | # of DocId's in a search result
  sizeSC           :: a -> Int
  sizeSC           = const 0

  -- | # of DocId's in a search result or infinity (Nothing) if too large
  sizeMaxSC        :: Int -> a -> Maybe Int
  sizeMaxSC mx x   = if mx' > mx then Nothing else Just mx'
                     where
                       mx' = sizeSC x

  -- | restrict a scored result
  --
  -- default: restricting a result does not change the score
  differenceSC     :: a -> a -> a

  -- | intersection of scored results
  intersectSC      :: a -> a -> a

  -- | intersection of scored results with respect to a context
  intersectDisplSC :: Int -> a -> a -> a
  intersectDisplSC _ = intersectSC

  -- | fuzzy intersection of scored results
  intersectFuzzySC :: Int -> Int -> a -> a -> a
  intersectFuzzySC _ _ = intersectSC

-- ------------------------------------------------------------
--
-- aggregating (raw) results for various types of scored results

class Aggregate a b where
    aggregate :: a -> b

-- | allow no aggregation

instance Aggregate a a where
    aggregate = id

-- ------------------------------------------------------------
--
-- | An arbitrary, maybe yet unscored result can be combined
-- with a score to a scored result

data Scored a = SCD !Score a
              deriving (Show)

instance Monoid a => Monoid (Scored a) where
  mempty
    = SCD mempty mempty

  SCD s1 x1 `mappend` SCD s2 x2
    = SCD (s1 <> s2) (x1 <> x2)

instance ScoredResult a => ScoredResult (Scored a) where
  s `boost` SCD s1 x1
    = SCD (s <> s1) x1

  nullSC (SCD s x)
    = nullSC s || nullSC x

  sizeSC (SCD _ s)
    = sizeSC s

  sizeMaxSC mx (SCD _ s)
    = sizeMaxSC mx s

  SCD s1 x1 `differenceSC` SCD _s2 x2
    = SCD s1 (x1 `differenceSC` x2)

  SCD s1 x1 `intersectSC` SCD s2 x2
    = SCD (s1 `intersectSC` s2) (x1 `intersectSC` x2)

  intersectDisplSC d (SCD s1 x1) (SCD s2 x2)
    = SCD (intersectDisplSC d s1 s2) (intersectDisplSC d x1 x2)

  intersectFuzzySC d1 d2 (SCD s1 x1) (SCD s2 x2)
    = SCD (intersectFuzzySC d1 d2 s1 s2) (intersectFuzzySC d1 d2 x1 x2)

-- ------------------------------------------------------------
