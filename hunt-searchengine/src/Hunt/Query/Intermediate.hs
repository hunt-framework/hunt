{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  The intermediate query results which have to be merged for the various combinatorial operations.

  'toResult' creates the final result which includes the document (and word) hits.
-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Intermediate
    ( ScoredResult(..)
    , Aggregate(..)

    , ScoredWords
    , ScoredContexts
    , ScoredOccs
    , ScoredRawDocs
    , ScoredCx
    , RankedDoc (..)

--    , toScoredDocs
    , boostAndAggregateCx
    , fromCxRawResults
    , fromRawResult
    , limitRawResult
    , limitCxRawResults
    , contextWeights
    , filterByDocSet
    , toDocsResult
    , toDocumentResultPage
    , toWordsResult

    , evalSequence
    , evalFollow
    , evalNear
    , evalOr
    , evalAnd
    , evalAndNot
    , evalBoost
    , evalPrim
    )
where

import           Prelude                     hiding (Word, null)

import           Control.Arrow               (second, (***))

import           Data.Aeson
import qualified Data.HashMap.Strict         as HM
import qualified Data.LimitedPriorityQueue   as Q
import qualified Data.List                   as L
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import qualified Hunt.Common.DocIdMap        as DM
import qualified Hunt.Common.DocIdMap.Packed as DMP
import           Hunt.Common.Document        (Document (..))
import           Hunt.Common.Occurrences     (Occurrences, DenseOccurrences)
import qualified Hunt.Common.Occurrences     as Occ
import qualified Hunt.Common.Positions       as Pos
import           Hunt.Index.Schema
import           Hunt.Query.Result           hiding (null)
import           Hunt.Scoring.Score
import           Hunt.Scoring.SearchResult

-- import           Debug.Trace

-- ------------------------------------------------------------

-- The result type for word search,
-- this is the required result type for completions search

newtype ScoredWords
    = SWS (Map Word Score)
      deriving (Show)

type ScoredContexts = ScoredWords

instance Monoid ScoredWords where
    mempty
        = SWS M.empty

    mappend (SWS m1) (SWS m2)
        = SWS $ M.unionWith (<>) m1 m2

instance ScoredResult ScoredWords where
    boost b (SWS m1)
        = SWS $ M.map (b *) m1

    nullSC (SWS m1)
        = M.null m1

    differenceSC (SWS m1) (SWS m2)
        = SWS $ M.difference m1 m2

    intersectSC (SWS m1) (SWS m2)
        = SWS $ M.intersectionWith (+) m1 m2

    intersectDisplSC _disp
        = intersectSC

    intersectFuzzySC _lb _ub
        = intersectSC

-- ------------------------------------------------------------

data ScoredOccs
    = SCO Score Occurrences
      deriving (Show)

instance Monoid ScoredOccs where
    mempty
        = SCO defScore DM.empty
    mappend (SCO s1 d1) (SCO s2 d2)
        = SCO ((s1 + s2) / 2.0) (DM.unionWith Pos.union d1 d2)

instance ScoredResult ScoredOccs where
    boost b (SCO s d)
        = SCO (b * s) d

    nullSC (SCO _s d)
        = DM.null d

    differenceSC (SCO s1 d1) (SCO _s2 d2)
        = SCO s1 (DM.difference d1 d2)

    intersectSC (SCO s1 d1) (SCO s2 d2)
        = SCO (s1 + s2) (DM.intersectionWith Pos.union d1 d2)

    intersectDisplSC disp (SCO s1 d1) (SCO s2 d2)
        = SCO (s1 + s2) (DM.intersectionWith (Pos.intersectionWithDispl disp) d1 d2)

    intersectFuzzySC lb ub (SCO s1 d1) (SCO s2 d2)
        = SCO (s1 + s2) (DM.intersectionWith (Pos.intersectionWithIntervall lb ub) d1 d2)

-- ------------------------------------------------------------

-- A result type for searching phrases and context search,
-- every word hit or word sequence hit is
-- associated with a score and a DocIdMap containing
-- the docs and position.
--
-- The positions are neccessary in phrase and context search,
-- afterwards the positions can be accumulated into a score,
-- e.g. by counting the occurences
--
-- A raw scored result can easily be converted into this type

newtype ScoredRawDocs
    = SRD [([Word], ScoredSearchResult)]
      deriving (Show)

instance Monoid ScoredRawDocs where
    mempty
        = SRD []
    mappend (SRD xs1) (SRD xs2)
        = SRD $ xs1 ++ xs2

instance ScoredResult ScoredRawDocs where
    boost b (SRD xs)
        = SRD $ L.map (second (boost b)) xs

    nullSC (SRD xs)
        = L.null xs

    differenceSC (SRD xs1) (SRD xs2)
        = srd $ [(ws1, SCD sc1 (diff occ1 xs2)) | (ws1, SCD sc1 occ1) <- xs1]
          where
            diff occ xs
                = L.foldl op occ xs
                  where
                    op occ' (_ws2, SCD _sc2 occ2)
                        = occ' `differenceSC` occ2

    intersectSC
        = intersectSRD intersectSC

    intersectDisplSC d
        = intersectSRD $ intersectDisplSC d

    intersectFuzzySC lb ub
        = intersectSRD $ intersectFuzzySC lb ub

-- ------------------------------------------------------------
--
-- auxiliary functions for ScoredRawDocs

-- | generalized intersection operator for ScoredRawDocs

intersectSRD :: (ScoredSearchResult -> ScoredSearchResult -> ScoredSearchResult)
              -> ScoredRawDocs -> ScoredRawDocs -> ScoredRawDocs
intersectSRD interSRD (SRD xs1) (SRD xs2)
    = srd $ [ (ws1 ++ ws2, interSRD socc1 socc2)
            | (ws1, socc1) <- xs1
            , (ws2, socc2) <- xs2
            ]

-- | smart constructor for ScoredRawDoc removing empty entries

srd :: [([Word], ScoredSearchResult)] -> ScoredRawDocs
srd
    = SRD . L.filter (not . nullSC . snd)

filterByDocSet :: UnScoredDocs -> ScoredRawDocs -> ScoredRawDocs
filterByDocSet (UDS ds) (SRD xs)
    = SRD $ concatMap filterDocs xs
      where
        filterDocs (ws, SCD sc occ)
            | nullSC occ'
                = []
            | otherwise
                = [(ws, SCD sc occ')]
            where
              occ' = occ `differenceSC` (mkSRfromUnScoredDocs ds)

-- ------------------------------------------------------------

-- | Add the Context dimension to a scored result

newtype ScoredCx a
    = SCX (Map Context a)
      deriving (Show)

instance Functor ScoredCx where
    fmap f (SCX m) = SCX $ M.map f m

instance Monoid a => Monoid (ScoredCx a) where
    mempty
        = SCX M.empty
    mappend (SCX m1) (SCX m2)
        = SCX $ M.unionWith (<>) m1 m2

instance ScoredResult a => ScoredResult (ScoredCx a) where
    boost b (SCX m)
        = SCX $ M.map (boost b) m

    nullSC (SCX m)
        = M.null m

    differenceSC
        = binopSCX differenceSC

    intersectSC
        = binopSCX intersectSC

    intersectDisplSC d
        = binopSCX $ intersectDisplSC d

    intersectFuzzySC lb ub
        = binopSCX $ intersectFuzzySC lb ub


-- | The final op for a search result: boost the partial results of the contexts
-- with the context weights from the schema and aggregate them
-- by throwing away the contexts

boostAndAggregateCx :: ScoredResult a => ScoredContexts -> ScoredCx a -> a
boostAndAggregateCx (SWS sm) (SCX m)
    = M.foldlWithKey op mempty m
      where
        op res k x
            = boost b x <> res
              where
                b = fromMaybe defScore $ M.lookup k sm

contextWeights :: Schema -> ScoredContexts
contextWeights s
    = SWS $ M.map cxWeight s

-- ------------------------------------------------------------
--
-- auxiliary functions for scored context results

binopSCX :: ScoredResult a => (a -> a -> a) -> (ScoredCx a -> ScoredCx a -> ScoredCx a)
binopSCX op (SCX m1) (SCX m2)
    = scx $ M.mapWithKey op' m1
          where
            op' cx1 sr1
                = sr1 `op` sr2
                  where
                    sr2 = fromMaybe mempty $ M.lookup cx1 m2


-- | smart constructore for ScoredCx removing empty contexts

scx :: ScoredResult a => Map Word a -> ScoredCx a
scx = SCX . M.filter (not . nullSC)

-- ------------------------------------------------------------
--
-- conversions from a raw scored result into scored results

type CxRawResults    = [(Context, RawScoredResult)]

type RawScoredResult = [(Word, (Score, SearchResult))]

fromCxRawResults :: CxRawResults -> ScoredCx ScoredRawDocs
fromCxRawResults crs
    = -- traceShow ("fromCxRawResults:"::String, crs, res) $
      res
      where
        res = mconcat $ L.map (uncurry $ fromRawResult) crs

fromRawResult :: Context -> RawScoredResult -> ScoredCx ScoredRawDocs
fromRawResult cx rr
    = SCX $ M.singleton cx sr
      where
        sr = SRD $ L.map toScored rr
            where
              toScored (w, (sc, occ))
                  = ([w], SCD sc occ)

limitCxRawResults :: Int -> CxRawResults -> CxRawResults
limitCxRawResults mx
    = L.map (second $ limitRawResult mx)

limitRawResult :: Int -> RawScoredResult -> RawScoredResult
limitRawResult maxDocs rs
    | maxDocs <= 0 = rs
    | otherwise = takeDocs maxDocs rs
    where
      takeDocs _  xs@[]
          = xs
      takeDocs _  xs@[_]
          = xs
      takeDocs mx (r@(_w, (_sc, occ)) : xs)
          = case sizeMaxSC mx occ of
              Nothing -> [r]
              Just s  -> let mx' = mx - s in
                         if mx' <= 0
                            then [r]
                            else r : takeDocs mx' xs

-- | convert a set of scored doc ids into a list of documents containing the score
--
-- This list may be further sorted by score, partitioned into pages or ...

toDocsResult :: (Applicative m, Monad m) =>
                (DocId -> Maybe Document) -> ScoredDocs -> m [RankedDoc]
toDocsResult dt (SDS m)
    = return $ fmap toDoc (DM.toList m)
      where
        toDoc (did, sc)
            = (toD . fromJust' . dt) did
            where
              toD d
                  = RD (wght d * sc, d)
        fromJust' (Just x) = x
        fromJust' Nothing  = error "Intermediate.toDocsResult: undefined"
{-# INLINE toDocsResult #-}

-- ----------------------------------------
-- ranking of documents

-- define a total ordering over documents by taking score and uri into account
-- this is used for ranking (only)
--
-- XXX TODO: maybe move to Common modules, since this type is now the regular
--           return type

newtype RankedDoc = RD {unRD :: (Score, Document)}
  deriving Show

instance Eq RankedDoc where
    (RD (sc1,d1)) == (RD (sc2,d2))
        = sc1 == sc2
          &&
          uri   d1 == uri   d2

instance Ord RankedDoc where
    (RD (sc1,d1)) `compare` (RD (sc2,d2))
        = sc1 `compare` sc2
          <>
          comparing uri d2 d1

instance ToJSON RankedDoc where
    toJSON (RD (c,d))
      = addScore $ toJSON d
      where
        addScore (Object hm)
          = Object $ HM.insert "score" (toJSON c) hm
        addScore _
          = error "toJSON rankedDoc: propably a bug introduced with #75"

toDocumentResultPage :: Int -> Int -> [RankedDoc] -> [RankedDoc]
toDocumentResultPage = Q.pageList

-- ----------------------------------------
-- ranking of completions

data RankedWord = RW Score Word
                   deriving Eq

instance Ord RankedWord where
    (RW s1 w1) `compare` (RW s2 w2)
        = s1 `compare` s2
          <>
          w2 `compare` w1

toWordsResult :: Int -> ScoredWords -> [(Word, Score)]
toWordsResult len (SWS m)
    = map (\ (RW s w) -> (w, s))
      . Q.toList 0 len
      . M.foldWithKey (\ w s -> Q.insert (RW s w)) (Q.mkQueue len)
      $ m

-- ------------------------------------------------------------

-- | aggregate scored occurences by counting the positions per doc and boost the
-- result with the score.
--
-- The positions in a doc are thrown away, so all kinds of phrase search become impossible

instance Aggregate ScoredOccs ScoredDocs where
    aggregate (SCO sc occ)
        = SDS $ DM.map toScore occ
          where
            toScore = (sc *) . mkScore . fromIntegral . Pos.size

-- | aggregate scored occurrences to unscored docs by throwing away the score
instance Aggregate ScoredOccs UnScoredDocs where
    aggregate (SCO _sc occ)
        = occurrencesToUnScoredDocs occ

-- | aggregate scored occurences to a score by aggregating first the positions and snd the doc ids
--
-- used in computing the score of word in completion search

instance Aggregate ScoredOccs Score where
    aggregate = agg2 . agg1
        where
          agg1 :: ScoredOccs -> ScoredDocs
          agg1 = aggregate
          agg2 :: ScoredDocs -> Score
          agg2 = aggregate

-- | aggregate scored raw results by throwing away the word hits and aggregating
-- all document hits by throwing away the positions.
--
-- The function for computing the scores of documents in a query

instance Aggregate ScoredRawDocs ScoredDocs where
    aggregate (SRD xs)
        = L.foldl (<>) mempty
          $ L.map (aggregate . snd) xs

-- | aggregate scored raw results into an unscored result by throwing away
-- the words, scores and occurrences

instance Aggregate ScoredRawDocs UnScoredDocs where
    aggregate (SRD xs)
        = L.foldl (<>) mempty
          $ L.map (aggregate . snd) xs

-- | aggregate the scored raw results by computing the score of all doc per word
--
-- Used in completion search.
-- The sequence of words (in a phrase) is cut to the last word (@L.last@). For this last one
-- the completions are computed.

instance Aggregate ScoredRawDocs ScoredWords where
    aggregate (SRD xs)
        = SWS
          $ L.foldl (\ res (w, sc) -> M.insertWith (+) w sc res) M.empty
          $ L.map (L.last *** aggregate) xs

-- | Lifting aggregation to scored context results

instance Aggregate a b => Aggregate (ScoredCx a) (ScoredCx b) where
    aggregate (SCX m)
        = SCX $ M.map aggregate m

-- ------------------------------------------------------------

-- query operator evaluation

-- | combine a sequence of results from a phrase query into a single result
-- and aggregate this result into a simpler structure,
-- e.g. a @ScoredDocs@ or @ScoredWords@ value
--
-- The arguments must be of this unaggregated from, still containing word
-- positions, else sequences of words are not detected

evalSequence :: (ScoredResult r, Aggregate ScoredRawDocs r) =>
                (ScoredCx ScoredRawDocs -> ScoredCx r) ->
                [ScoredCx ScoredRawDocs] -> ScoredCx r
evalSequence _aggr []
    = mempty
evalSequence aggr [r1]
    = aggr r1
evalSequence aggr (r1 : rs)
    = aggr $ L.foldl op r1 (L.zip [(1::Int)..] rs)
      where
        op acc (d, r2) = intersectDisplSC d acc r2

evalFollow :: (ScoredResult r, Aggregate ScoredRawDocs r) =>
              (ScoredCx ScoredRawDocs -> ScoredCx r) ->
              Int ->
              [ScoredCx ScoredRawDocs] -> ScoredCx r
evalFollow _aggr _d []
    = mempty
evalFollow aggr _d [r1]
    = aggr r1
evalFollow aggr dp (r1 : rs)
    = aggr $ L.foldl op r1 (L.zip [dp, (2 * dp) ..] rs)
      where
        op acc (d, r2) = intersectFuzzySC 1 d acc r2

evalNear :: (ScoredResult r, Aggregate ScoredRawDocs r) =>
            (ScoredCx ScoredRawDocs -> ScoredCx r) ->
            Int ->
           [ScoredCx ScoredRawDocs] -> ScoredCx r
evalNear _aggr _d []
    = mempty
evalNear aggr _d [r1]
    = aggr r1
evalNear aggr dp (r1 : rs)
    = aggr $ L.foldl op r1 (L.zip [dp, (2 * dp) ..] rs)
      where
        op acc (d, r2) = intersectFuzzySC (-d) d acc r2

evalOr, evalAnd, evalAndNot :: ScoredResult a => [a] -> a
evalOr     = evalBinary (<>)
evalAnd    = evalBinary intersectSC
evalAndNot = evalBinary differenceSC

evalBinary :: ScoredResult a => (a -> a -> a) -> [a] -> a
evalBinary _ []
    = mempty
evalBinary _ [r]
    = r
evalBinary op rs
    = L.foldl1 op rs

evalBoost :: ScoredResult a => Score -> a -> a
evalBoost = boost

evalPrim :: Aggregate (ScoredCx ScoredRawDocs) (ScoredCx r) => ScoredCx ScoredRawDocs -> ScoredCx r
evalPrim = aggregate

-- ------------------------------------------------------------
