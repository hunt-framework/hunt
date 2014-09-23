-- ----------------------------------------------------------------------------
{- |
  Module     : Hunt.Query.Ranking
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  The ranking mechanism for Hunt.

  Customized ranking functions for both documents and suggested words can be
  provided by the user. Some predefined ranking functions are avaliable, too.

-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Ranking
  (
  -- * Ranking types
    RankConfig (..)
  , DocRanking
  , WordRanking
  , ContextWeights

  -- * Ranking
  , rank

  -- * Predefined document rankings
  , docRankByCount
  --, docRankWeightedByCount

  -- * Predefined word rankings
  , wordRankByCount
  , wordRankBySimilarity
  --, wordRankWeightedByCount

  , defaultRankConfig
  )
where

import           Prelude               hiding (foldr)

import           Control.Applicative

--import           Data.Foldable
--import           Data.Function
import           Data.Maybe

import qualified Data.List             as L
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId     (DocId)
import qualified Hunt.Common.DocIdMap  as DM
import           Hunt.Common.Document  (DocumentWrapper (..), Document (..))
import           Hunt.Common.Positions as Pos
import           Hunt.DocTable         as Dt
import           Hunt.Query.Result
import           Hunt.Scoring.Score    (mkScore, noScore, defScore, toDefScore)
import           Hunt.Utility

-- import           Debug.Trace

-- ------------------------------------------------------------

-- | The configuration of the ranking mechanism.
data RankConfig e
  = RankConfig
    { docRanking  :: DocRanking e   -- ^ A function to determine the score of a document.
    , wordRanking :: WordRanking    -- ^ A function to determine the score of a word.
    }

-- | The signature of a function to determine the score of a document.
type DocRanking e = ContextWeights -> DocId -> Score -> DocInfo e -> DocContextHits -> Score

-- TODO: add ContextWeights
-- | The signature of a function to determine the score of a word.
type WordRanking  = Word -> WordInfo -> WordContextHits -> Score

-- | Weights for the contexts (optional).
type ContextWeights = Map Context Score

-- ------------------------------------------------------------

-- | The configuration of the ranking mechanism.
defaultRankConfig :: DocumentWrapper e => RankConfig e
defaultRankConfig
    = RankConfig
      { docRanking  = docRankByCount
      , wordRanking = wordRankBySimilarity
      -- , wordRanking = wordRankByCount
      }

-- ------------------------------------------------------------

-- | Rank the result with custom ranking functions (and the given context weights).

rank :: (DocTable dt, Monad m, Applicative m) =>
        RankConfig e -> dt -> ContextWeights -> Result e -> m (Result e)

rank (RankConfig fd fw {-ld lw-}) dt cw r
  = do sdh <- scoredDocHits
       return $ Result sdh scoredWordHits
  where
    scoredDocHits
        = DM.traverseWithKey
          (\k (di, dch) ->
               do
                 kw <- maybe defScore (wght . unwrap) <$> Dt.lookup k dt
                 return $ ( setDocScore (fd cw k kw di dch) di
                          , dch
                          )
          ) $ docHits r

    scoredWordHits
        = M.mapWithKey
          ( \ k (WIH wi wch) -> WIH (setWordScore (fw k wi wch) wi) wch )
          $ wordHits r

-- ------------------------------------------------------------

-- | Rank documents by count and multiply occurrences with
--   their respective context weights (default @1.0@).
--   Pass an empty map to discard context weights.
--
-- @docRankByCount :: ContextWeights -> DocId -> Score -> DocInfo e -> DocContextHits -> Score@

docRankByCount :: DocRanking e
docRankByCount cw _ docWeight di h
    = docBoost di * docWeight * scHits
    where
      scHits = M.foldrWithKey
               (\cx -> (cxw cx *)
                       .::
                       flip (M.foldr (\h2 r2 -> mkScore (fromIntegral (Pos.size h2)) + r2))
               ) noScore h

      cxw cx = fromMaybe defScore $ M.lookup cx cw

-- | Rank words by count.
--
-- @wordRankByCount :: Word -> WordInfo -> WordContextHits -> Score@
wordRankByCount :: WordRanking
wordRankByCount _w _i h
  = countHits h

wordRankBySimilarity :: WordRanking
wordRankBySimilarity wordFound (WordInfo searchTerms sc) wch
    = cnt * toDefScore sc * simBoost
    where
      cnt = countHits wch

      simBoost :: Score
      simBoost
          | L.null searchTerms            -- make simBoost total
              = noScore
          | otherwise
              = maximum (L.map (similar wordFound) searchTerms)

      similar :: Text -> Text -> Score
      similar found searched
          | searched == found
              = boostExactHit
          | ls == lf
              = boostSameLength
          | ls < lf                     -- reduce score by length of found word
              = fromIntegral ls / fromIntegral lf
          | otherwise                   -- make similar total
              = noScore
          where
            boostExactHit   = 10.0
            boostSameLength =  5.0      -- NoCase hits

            ls = T.length searched
            lf = T.length found

countHits :: WordContextHits -> Score
countHits wch
    = M.foldr op 0.0 wch
    where
      op x res = DM.foldr (+) 0.0 x + res

-- ------------------------------------------------------------

-- The old weighting mechanism
--   - used a list of context weights
--   - normalizes the weights to a maximum of 1.0
--     - not sure if this is really necessary or the users responsibility
--     - the computation should be done /once/ every time the weights are set, not with every query
--   - seems overall cumbersome

{-
-- | Rank documents by context-weighted count. The weights will be normalized to a maximum of 1.0.
--   Contexts with no weight (or a weight of zero) will be ignored.
docRankWeightedByCount :: [(Context, Score)] -> DocId -> DocInfo e -> DocContextHits -> Score
docRankWeightedByCount ws _ _
  =  M.foldrWithKey (calcWeightedScore ws) 0.0

-- | Rank words by context-weighted count. The weights will be normalized to a maximum of 1.0.
--   Contexts with no weight (or a weight of zero) will be ignored.
wordRankWeightedByCount :: [(Context, Score)] -> Word -> WordInfo -> WordContextHits -> Score
wordRankWeightedByCount ws _ _
  = M.foldrWithKey (calcWeightedScore ws) 0.0

-- | Calculate the weighted score of occurrences of a word.
calcWeightedScore :: Foldable f => [(Context, Score)] -> Context -> f Positions -> Score -> Score
calcWeightedScore ws c h r
  = maybe r (\w -> r + ((w / mw) * count)) $ lookupWeight c ws
  where
  count = fromIntegral $ foldl' (flip $ (+) . Pos.size) 0 h
  mw    = snd $ L.maximumBy (compare `on` snd) ws

-- | Find the weight of a context in a list of weights. If the context was not found or it's
--   weight is equal to zero, 'Nothing' will be returned.
lookupWeight :: Context -> [(Context, Score)] -> Maybe Score
lookupWeight _ []     = Nothing
lookupWeight c (x:xs)
  = if fst x == c
    then
      if snd x /= 0.0
      then Just (snd x)
      else Nothing
    else lookupWeight c xs
-}

-- ------------------------------------------------------------
