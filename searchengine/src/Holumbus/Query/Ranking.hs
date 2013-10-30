-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Ranking
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  The ranking mechanism for Holumbus.

  Customized ranking functions for both documents and suggested words can be
  provided by the user. Some predefined ranking functions are avaliable, too.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Ranking
  (
  -- * Ranking types
  RankConfig (..)
  , DocRanking
  , WordRanking

  -- * Ranking
  , rank

  -- * Predefined document rankings
  , docRankByCount
  , docRankWeightedByCount

  -- * Predefined word rankings
  , wordRankByCount
  , wordRankWeightedByCount
  )
where

import           Prelude                           hiding (foldr)

import           Data.Foldable
import           Data.Function

import qualified Data.List                         as L
import qualified Data.Map                          as M

import           Holumbus.Common
import qualified Holumbus.Common.DocIdMap    as DM
import           Holumbus.Query.Result
import           Holumbus.Common.Positions
-- ----------------------------------------------------------------------------

-- | The configuration of the ranking mechanism.
data RankConfig e       = RankConfig
                          { docRanking :: DocRanking e   -- ^ A function to determine the score of a document.
                          , wordRanking :: WordRanking  -- ^ A funciton to determine the score of a word.
                          }

-- | The signature of a function to determine the score of a document.
type DocRanking e       = DocId -> DocInfo e -> DocContextHits -> Score

-- | The signature of a function to determine the score of a word.
type WordRanking        = Word -> WordInfo -> WordContextHits -> Score

-- ----------------------------------------------------------------------------

-- | Rank the result with custom ranking functions.
rank                    :: RankConfig e -> Result e -> Result e
rank (RankConfig fd fw {-ld lw-}) r
                        = Result scoredDocHits scoredWordHits
  where
  scoredDocHits         = DM.mapWithKey (\k (di, dch) -> (setDocScore (fd k di dch) di, dch)) $
                          docHits r
  scoredWordHits        = M.mapWithKey (\k (wi, wch) -> (setWordScore (fw k wi wch) wi, wch)) $
                          wordHits r

-- | Rank documents by count.
docRankByCount          :: DocId -> DocInfo e -> DocContextHits -> Score
docRankByCount _ _ h    = fromIntegral $
                          M.foldr (flip (M.foldr (\h2 r2 -> sizePos h2 + r2))) 0 h

-- | Rank words by count.
wordRankByCount         :: Word -> WordInfo -> WordContextHits -> Score
wordRankByCount _ _ h   = fromIntegral $ M.foldr (flip (DM.foldr ((+) . sizePos))) 0 h

-- | Rank documents by context-weighted count. The weights will be normalized to a maximum of 1.0.
-- Contexts with no weight (or a weight of zero) will be ignored.
docRankWeightedByCount  :: [(Context, Score)] -> DocId -> DocInfo e -> DocContextHits -> Score
docRankWeightedByCount ws _ _
                        =  M.foldrWithKey (calcWeightedScore ws) 0.0

-- | Rank words by context-weighted count. The weights will be normalized to a maximum of 1.0.
-- Contexts with no weight (or a weight of zero) will be ignored.
wordRankWeightedByCount :: [(Context, Score)] -> Word -> WordInfo -> WordContextHits -> Score
wordRankWeightedByCount ws _ _
                        = M.foldrWithKey (calcWeightedScore ws) 0.0

-- | Calculate the weighted score of occurrences of a word.
calcWeightedScore       :: (Foldable f) =>
                           [(Context, Score)] -> Context -> f Positions -> Score -> Score
calcWeightedScore ws c h r
                        = maybe r (\w -> r + ((w / mw) * count)) $
                          lookupWeight c ws
  where
    count               = fromIntegral $
                          foldl' (flip $ (+) . sizePos) 0 h
    mw                  = snd $
                          L.maximumBy (compare `on` snd) ws

-- | Find the weight of a context in a list of weights. If the context was not found or it's
-- weight is equal to zero, 'Nothing' will be returned.
lookupWeight            :: Context -> [(Context, Score)] -> Maybe Score
lookupWeight _ []       = Nothing
lookupWeight c (x:xs)   = if fst x == c
                          then if snd x /= 0.0
                               then Just (snd x)
                               else Nothing
                          else lookupWeight c xs

-- ----------------------------------------------------------------------------
