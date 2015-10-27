{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- ----------------------------------------------------------------------------

module Hunt.Scoring.SearchResult
where

import           Data.Foldable
import           Data.Monoid

import           Hunt.Common.BasicTypes  (Position)
import           Hunt.Common.DocIdMap    (DocIdMap)
import qualified Hunt.Common.DocIdMap    as DM
import qualified Hunt.Common.DocIdMap.Packed as DMP
import           Hunt.Common.DocIdSet    (DocIdSet)
import qualified Hunt.Common.DocIdSet    as DS
import           Hunt.Common.Occurrences (Occurrences, DenseOccurrences)
import qualified Hunt.Common.Occurrences as Occ
import qualified Hunt.Common.Positions   as Pos
import           Hunt.Scoring.Score

import           Prelude                 as P

-- ------------------------------------------------------------
--
-- One type of search result is a set of doc ids assosiated with the
-- set of positions where a word has occuref

newtype Occurrences'
    = OCC DenseOccurrences
      deriving (Eq, Show)

instance Monoid Occurrences' where
    mempty
        = OCC DMP.empty
    mappend (OCC d1) (OCC d2)
        = OCC $ DMP.unionWith Pos.union d1 d2

instance ScoredResult Occurrences' where
    boost _b
        = id

    sizeSC (OCC d)
      = DMP.size d

    sizeMaxSC mx (OCC d)
      = DMP.sizeWithLimit mx d

    nullSC (OCC d)
        = DMP.null d

    differenceSC (OCC d1) (OCC d2)
        = OCC $ DMP.difference d1 d2

    intersectSC (OCC d1) (OCC d2)
        = OCC $ DMP.intersectionWith Pos.union d1 d2

    intersectDisplSC disp (OCC d1) (OCC d2)
        = OCC $ DMP.intersectionWith (Pos.intersectionWithDispl disp) d1 d2

    intersectFuzzySC lb ub (OCC d1) (OCC d2)
        = OCC $ DMP.intersectionWith (Pos.intersectionWithIntervall lb ub) d1 d2


-- | "upcast": all DocId's get a score of 1.0
docIdsToOccurrences' :: DocIdSet -> Occurrences'
docIdsToOccurrences'
  = OCC . DMP.fromDocIdSet (\_ -> Pos.singleton 1)

-- | "upcast": all scored DocId's get the # of positions equivalent to the score
--
-- a bit artificial, but scoring is not destroied
scoredDocsToOccurrences' :: DMP.DocIdMap Score -> Occurrences'
scoredDocsToOccurrences'
  = OCC . DMP.map scoreToOcc
    where
      scoreToOcc sc
        = Pos.fromList [(1::Position) .. (floor . maybe 1.0 id . getScore $ sc)]

-- ------------------------------------------------------------

-- The result type for document search: every doc id is associated with a score

newtype ScoredDocs
    = SDS (DMP.DocIdMap Score)
      deriving (Eq, Show)

instance Monoid ScoredDocs where
    mempty
        = SDS DMP.empty
    mappend (SDS m1) (SDS m2)
        = SDS $ DMP.unionWith (<>) m1 m2

instance ScoredResult ScoredDocs where
    boost b x@(SDS m1)
      | b == defScore = x
      | otherwise     = SDS $ DMP.map (b *) m1

    nullSC (SDS m1)
        = DMP.null m1

    sizeSC (SDS m)
      = DMP.size m

    sizeMaxSC mx (SDS m)
      = DMP.sizeWithLimit mx m

    differenceSC (SDS m1) (SDS m2)
        = SDS $ DMP.difference m1 m2

    intersectSC (SDS m1) (SDS m2)
        = SDS $ DMP.intersectionWith (+) m1 m2

    intersectDisplSC _disp
        = intersectSC

    intersectFuzzySC _lb _ub
        = intersectSC

-- | aggregate scored docs to a single score by summing up the scores and throw away the DocIds

instance Aggregate ScoredDocs Score where
    aggregate (SDS m)
        = foldl' (<>) defScore m

-- | "downcast": the set of positions is aggregated into a ScoredDocs
occurrencesToScoredDocs :: DenseOccurrences -> ScoredDocs
occurrencesToScoredDocs
  = SDS . occurrencesToDocIdMapScore

-- | "downcast": the set of positions is aggregated into DocIdMap with frequency count
occurrencesToDocIdMapScore :: DenseOccurrences -> DMP.DocIdMap Score
occurrencesToDocIdMapScore
  = DMP.map toScore
  where
    toScore = mkScore . fromIntegral . Pos.size


-- | "upcast": all DocId's get a score of 1.0
docIdsToScoredDocs :: DocIdSet -> ScoredDocs
docIdsToScoredDocs
  = SDS . DMP.fromDocIdSet (\_ -> defScore)

scoredDocsToDocIdSet :: ScoredDocs -> DocIdSet
scoredDocsToDocIdSet (SDS dim)
  = DMP.keys dim

-- ------------------------------------------------------------

-- | The result type for unscored search of documents
--
-- used when all matching docs must be processed,
-- e.g. in DeleteByQuery commands
-- and as a variant in SearchResult, when there are only DocId's
-- stored in an index

newtype UnScoredDocs
    = UDS DocIdSet
      deriving (Eq, Show, Monoid)

instance ScoredResult UnScoredDocs where
    boost _b uds
        = uds

    nullSC (UDS s)
        = DS.null s

    sizeSC (UDS s)
      = DS.size s

    {-
    sizeMaxSC mx (UDS s)
      = DS.sizeWithLimit mx s   -- not supported by Data.IntSet
    -}

    differenceSC (UDS s1) (UDS s2)
        = UDS $ DS.difference s1 s2

    intersectSC (UDS s1) (UDS s2)
        = UDS $ DS.intersection s1 s2

    intersectDisplSC _disp
        = intersectSC

    intersectFuzzySC _lb _ub
        = intersectSC

occurrencesToUnScoredDocs :: DenseOccurrences -> UnScoredDocs
occurrencesToUnScoredDocs
  = UDS . DMP.keys

scoredDocsToUnScoredDocs :: DMP.DocIdMap Score -> UnScoredDocs
scoredDocsToUnScoredDocs
  = UDS . DMP.keys

unScoredDocsToDocIdSet :: UnScoredDocs -> DocIdSet
unScoredDocsToDocIdSet (UDS ds) = ds

-- ------------------------------------------------------------

-- | The result type for all variants of search results
--
-- Indexes can store Occurrences, sets of DocId's, and sets of DocId's
-- associated with a frequency count (ScoredDocs)
-- This type again is an instance of ScoredResult.
--
-- The case analysis in the combinators can give us
-- performance, most of the otherwise necessary conversions
-- can be eliminated

-- ------------------------------------------------------------

data SearchResult
  = ROC {unROC :: Occurrences'}
  | RSD {unRSD :: ScoredDocs  }
  | RUD {unRUD :: UnScoredDocs}
    deriving (Eq, Show)

mkSRfromOccurrences :: DenseOccurrences -> SearchResult
mkSRfromOccurrences = ROC . OCC

mkSRfromScoredDocs :: DMP.DocIdMap Score -> SearchResult
mkSRfromScoredDocs = RSD . SDS

mkSRfromUnScoredDocs :: DocIdSet -> SearchResult
mkSRfromUnScoredDocs = RUD . UDS

instance Monoid SearchResult where
  mempty        = RUD mempty

  x1 `mappend` x2
    | nullSC x1 = x2
    | nullSC x2 = x1
    | otherwise = mappend0 x1 x2

instance ScoredResult SearchResult where
  boost _b       = id

  nullSC (RUD x) = nullSC x
  nullSC (RSD x) = nullSC x
  nullSC (ROC x) = nullSC x

  sizeSC (RUD x) = sizeSC x
  sizeSC (RSD x) = sizeSC x
  sizeSC (ROC x) = sizeSC x

  sizeMaxSC mx (RUD x) = sizeMaxSC mx x
  sizeMaxSC mx (RSD x) = sizeMaxSC mx x
  sizeMaxSC mx (ROC x) = sizeMaxSC mx x

  differenceSC x1 x2
    | nullSC x2 = x1
    | nullSC x1 = mempty
    | otherwise = diff0 x1 (srToRUD x2)

  intersectSC x1 x2
    | nullSC x1 = mempty
    | nullSC x2 = mempty
    | otherwise = inters0 x1 x2

  intersectDisplSC n x1 x2
    | nullSC x1 = mempty
    | nullSC x2 = mempty
    | otherwise = interd0 n x1 x2

  intersectFuzzySC n m x1 x2
    | nullSC x1 = mempty
    | nullSC x2 = mempty
    | otherwise = interf0 n m x1 x2

searchResultToUnScoredDocs :: SearchResult -> UnScoredDocs
searchResultToUnScoredDocs = unRUD . srToRUD

searchResultToScoredDocs :: SearchResult -> ScoredDocs
searchResultToScoredDocs = unRSD . srToRSD

searchResultToOccurrences :: SearchResult -> DenseOccurrences
searchResultToOccurrences = (\ (OCC x) -> x) . unROC . srToROC

-- ------------------------------------------------------------

-- | union

mappend0 :: SearchResult -> SearchResult -> SearchResult
RUD x1 `mappend0` RUD x2       = RUD $ x1 <> x2

RSD x1 `mappend0` RSD x2       = RSD $ x1 <> x2
RSD x1 `mappend0` RUD (UDS x2) = RSD $ x1 <> docIdsToScoredDocs x2

ROC x1 `mappend0` ROC x2       = ROC $ x1 <> x2
ROC x1 `mappend0` RUD (UDS x2) = ROC $ x1 <> docIdsToOccurrences' x2
ROC x1 `mappend0` RSD (SDS x2) = ROC $ x1 <> scoredDocsToOccurrences' x2

x1     `mappend0` x2           = x2 `mappend0` x1


-- | difference

diff0 :: SearchResult -> SearchResult -> SearchResult
RUD x1        `diff0` RUD x2        = RUD $ x1 `differenceSC` x2
RSD (SDS dm1) `diff0` RUD (UDS ds2) = RSD (SDS $ DMP.diffWithSet dm1 ds2)
ROC (OCC os1) `diff0` RUD (UDS ds2) = ROC (OCC $ DMP.diffWithSet os1 ds2)
_x1           `diff0` _x2           = error "SearchResult.diff0: illegal variants in args"


-- | intersection

inters0 :: SearchResult -> SearchResult -> SearchResult
RUD x1        `inters0` RUD x2        = RUD $ x1 `intersectSC` x2

RSD x1        `inters0` RSD x2        = RSD $ x1 `intersectSC` x2
RSD (SDS dm1) `inters0` RUD (UDS ds2) = RSD (SDS $ DMP.intersectionWithSet dm1 ds2)
ROC x1        `inters0` ROC x2        = ROC $ x1 `intersectSC` x2
ROC (OCC os1) `inters0` RUD (UDS ds2) = ROC (OCC $ DMP.intersectionWithSet os1 ds2)
ROC (OCC os1) `inters0` RSD (SDS ds2) = ROC (OCC $ DMP.intersectionWith (\ x _ -> x) os1 ds2)

x1            `inters0` x2            = x2 `inters0` x1


-- | intersection with displ,
-- positions are needed to do something else than an intersection

interd0 :: Int -> SearchResult -> SearchResult -> SearchResult
interd0 n (ROC x1) (ROC x2) = ROC $ intersectDisplSC n x1 x2
interd0 _ x1       x2       = x1 `inters0` x2


-- | intersection within an intervall,
-- positions are needed to do something else than an intersection

interf0 :: Int -> Int -> SearchResult -> SearchResult -> SearchResult
interf0 n m (RUD x1) (RUD x2) = RUD $ intersectFuzzySC n m x1 x2
interf0 _ _ x1       x2       = x1 `inters0` x2

-- conversion into all variants

srToRUD :: SearchResult -> SearchResult
srToRUD (RSD (SDS x)) = RUD $ scoredDocsToUnScoredDocs  x    -- downcast
srToRUD (ROC (OCC x)) = RUD $ occurrencesToUnScoredDocs x    -- downcast
srToRUD x             = x

srToRSD :: SearchResult -> SearchResult
srToRSD (ROC (OCC x)) = RSD $ occurrencesToScoredDocs x      -- downcast
srToRSD (RUD (UDS x)) = RSD $ docIdsToScoredDocs      x      -- upcast
srToRSD x             = x

srToROC :: SearchResult -> SearchResult
srToROC (RSD (SDS x)) = ROC $ scoredDocsToOccurrences' x     -- upcast
srToROC (RUD (UDS x)) = ROC $ docIdsToOccurrences'     x     -- upcast
srToROC x             = x

{-- not yet used

lsSR :: SearchResult -> SearchResult -> Bool
lsSR (RUD _) (RSD _) = True
lsSR (RUD _) (ROC _) = True
lsSR (RSD _) (ROC _) = True
lsSR _       _       = False

upcastSR :: SearchResult -> SearchResult -> SearchResult
upcastSR x1 x2
  | not (x1 `lsSR` x2) = x1
upcastSR x1   (RUD _)  = srToRUD x1
upcastSR x1   (RSD _)  = srToRSD x1
upcastSR x1   (ROC _)  = srToROC x1

-- | this op converts the arguments to the least general type
-- before applying the real operation, so the op has only to deal with
-- equal variants

comb2SR :: (SearchResult -> SearchResult -> SearchResult) ->
           SearchResult -> SearchResult -> SearchResult
comb2SR op x1 x2
  = x1' `op` x2'
  where
    x1' = upcastSR x1 x2
    x2' = upcastSR x2 x1'
-- -}
-- ------------------------------------------------------------

-- | A search result with an attached score

type ScoredSearchResult
  = Scored SearchResult

-- | aggregate scored occurences by counting the positions per doc and boost the
-- result with the score.
--
-- The positions in a doc are thrown away, so all kinds of phrase search become impossible

instance Aggregate ScoredSearchResult ScoredDocs where
    aggregate (SCD sc sr)
        = boost sc $ searchResultToScoredDocs sr

instance Aggregate ScoredSearchResult UnScoredDocs where
    aggregate (SCD sc sr)
        = boost sc $ searchResultToUnScoredDocs sr

instance Aggregate ScoredSearchResult Score where
    aggregate
      = aggr2 . aggr1
      where
        aggr1 :: ScoredSearchResult -> ScoredDocs
        aggr1 = aggregate
        aggr2 :: ScoredDocs -> Score
        aggr2 = aggregate

scoredSearchResultToScoredDocs :: ScoredSearchResult -> ScoredDocs
scoredSearchResultToScoredDocs (SCD s (RSD x))
  = boost s x

scoredSearchResultToScoredDocs (SCD s (ROC (OCC x)))
  = boost s . occurrencesToScoredDocs $ x

scoredSearchResultToScoredDocs (SCD s (RUD (UDS x)))
  = boost s . docIdsToScoredDocs $ x

scoredSearchResultToUnScoredDocs :: ScoredSearchResult -> UnScoredDocs
scoredSearchResultToUnScoredDocs (SCD s (RUD x))
  = boost s x

scoredSearchResultToUnScoredDocs (SCD s (ROC (OCC x)))
  = boost s . occurrencesToUnScoredDocs $ x

scoredSearchResultToUnScoredDocs (SCD s (RSD (SDS x)))
  = boost s . scoredDocsToUnScoredDocs $ x

-- ------------------------------------------------------------

srDiffDocs :: DocIdSet -> SearchResult -> SearchResult
srDiffDocs dIds sr
  | DS.null dIds = sr
  | otherwise     = case sr of
                     ROC (OCC occ') -> ROC (OCC (DMP.diffWithSet occ' dIds))
                     RSD (SDS sds)  -> RSD (SDS (DMP.diffWithSet sds dIds ))
                     RUD (UDS uds)  -> RUD (UDS (DS.difference uds dIds))

srNull :: SearchResult -> Bool
srNull (ROC (OCC occ')) = DMP.null occ'
srNull (RSD (SDS sds))  = DMP.null sds
srNull (RUD (UDS uds))  = DS.null uds
