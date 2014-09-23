-- ----------------------------------------------------------------------------

{- |
  Basic operations for scoring keys
-}

-- ----------------------------------------------------------------------------

module Hunt.Scoring.Keys
where

import           Control.Arrow                   (second)

import qualified Data.List                       as L
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Hunt.Index.Schema.Normalize.Int (normalizeToInt')
import           Hunt.Scoring.Score

-- ------------------------------------------------------------
--
-- | Don't compare the search key with the result keys.
--
-- All results have the same quality

addDefScore :: [(a, b)] -> [(a, (Score, b))]
addDefScore = L.map (second (\ x -> (defScore, x)))

-- ------------------------------------------------------------

-- | a simple similarity heuristic for scoring words found
-- when doing a fuzzy or prefix search

similar :: Text -> Text -> Score
similar s f
    = -- traceShow ("similar"::Text, s, f, r) $
      r
    where
      r = similar' s f

similar' :: Text -> Text -> Score
similar' searched found
    | searched == found
        = 1.0
    | ls == lf
        = 0.75
    | ls < lf                     -- reduce score by length of found word
        = 0.5 * (fromIntegral ls / fromIntegral lf)
    | otherwise                   -- make similar total
        = noScore
    where
      ls = T.length searched
      lf = T.length found

-- ------------------------------------------------------------

similarInt :: Text -> Text -> Score
similarInt searched found
    = fromMaybe noScore $
      do s <- normalizeToInt' searched
         f <- normalizeToInt' found
         return $ similarInt' s f

similarInt' :: Int -> Int -> Score
similarInt' x y
  = similarFloat (fromIntegral (x::Int)) (fromIntegral (y::Int))
    
similarRangeInt :: Text -> Text -> Text -> Score
similarRangeInt lbt ubt found
    = fromMaybe noScore $
      do lb <- normalizeToInt' lbt
         ub <- normalizeToInt' ubt
         f  <- normalizeToInt' found
         return $ similarRangeInt' lb ub f

similarRangeInt' :: Int -> Int -> Int -> Score
similarRangeInt' lb ub found
  = similarFloat
    (fromIntegral ((lb::Int) + (ub::Int)) / 2.0)
    (fromIntegral (found::Int))

similarFloat :: Float -> Float -> Score
similarFloat mu
    = mkScore . bellCurve (sigma mu) mu

sigma :: Float -> Float
sigma x
    = abs x `max` 10.0 / 10.0

-- | Gaussian bell curve for scoring
bellCurve :: Float -> (Float -> Float -> Float)
bellCurve sigma'
    = \ mu x -> exp (- (x - mu) ^ _2 / sigma2'2)
    where
      _2 :: Int
      _2 = 2
      sigma2'2 = 2.0 * sigma' ^ _2

-- ------------------------------------------------------------



