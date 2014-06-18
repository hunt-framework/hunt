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
(
  -- * The intermediate result type.
    Intermediate
  , IntermediateContexts
  , IntermediateWords

  -- * Construction
  , empty

  -- * Query
  , null
  , size

  -- * Combine
  , union
  , merge
  , difference
  , intersection
  , unions
  , unionsDocLimited
  , merges
  , mergesDocLimited
--  , intersections1
--  , differences1

  -- * Conversion
  , fromList
  , fromListCxs
  , toResult

  , ScoredResult(..)
  , Aggregate(..)

  , ScoredDocs
  , ScoredWords
  , ScoredContexts
  , ScoredOccs
  , ScoredRawDocs
  , ScoredCx
  , UnScoredDocs

  , toScoredDocs
  , boostAndAggregateCx
  , fromCxRawResults
  , fromRawResult
  , limitRawResult
  , limitCxRawResults
  , contextWeights
  , toDocsResult
  , sortByScore

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

import           Prelude                 hiding (null)
import qualified Prelude                 as P

import           Control.Applicative     hiding (empty)
import           Control.Arrow           (second, (***))

import           Data.Function           (on)
import qualified Data.List               as L
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe
-- import           Data.Text             (Text)
-- import qualified Data.Text             as T
import           Hunt.Query.Result       hiding (null)

import           Hunt.Common
import qualified Hunt.Common.DocIdMap    as DM
import qualified Hunt.Common.DocIdSet    as DS
import           Hunt.Common.Document    (DocumentWrapper (..), emptyDocument)
import           Hunt.Common.Occurrences (intersectOccurrences)
import qualified Hunt.Common.Positions   as Pos
import           Hunt.DocTable           (DocTable)
import qualified Hunt.DocTable           as Dt

-- import           Debug.Trace

-- ------------------------------------------------------------
--
-- The Monoid @mempty@ acts as empty result,
-- (<>) is the union of scored results

class Monoid a => ScoredResult a where
    boost            :: Score -> a -> a
    nullSC           :: a -> Bool
    differenceSC     :: a -> a -> a
    intersectSC      :: a -> a -> a
    intersectDisplSC :: Int -> a -> a -> a
    intersectFuzzySC :: Int -> Int -> a -> a -> a

-- ------------------------------------------------------------

-- The result type for document search, every doc is associated with a score

newtype ScoredDocs
    = SDS (DocIdMap Score)
      deriving (Show)

instance Monoid ScoredDocs where
    mempty
        = SDS DM.empty
    mappend (SDS m1) (SDS m2)
        = SDS $ DM.unionWith (<>) m1 m2

instance ScoredResult ScoredDocs where
    boost b (SDS m1)
        = SDS $ DM.map (b *) m1

    nullSC (SDS m1)
        = DM.null m1

    differenceSC (SDS m1) (SDS m2)
        = SDS $ DM.difference m1 m2

    intersectSC (SDS m1) (SDS m2)
        = SDS $ DM.intersectionWith (+) m1 m2

    intersectDisplSC _disp
        = intersectSC

    intersectFuzzySC _lb _ub
        = intersectSC

toScoredDocs :: Occurrences -> ScoredDocs
toScoredDocs os
    = SDS $ DM.map toScore os
      where
        toScore = mkScore . fromIntegral . Pos.size

-- ------------------------------------------------------------

-- | The result type for unscored search of documents
--
-- used when all matching docs must be processed,
-- e.g. in DeleteByQuery commands

newtype UnScoredDocs
    = UDS DS.DocIdSet
      deriving (Show, Monoid)

instance ScoredResult UnScoredDocs where
    boost b uds
        = uds

    nullSC (UDS s)
        = DS.null s

    differenceSC (UDS s1) (UDS s2)
        = UDS $ DS.difference s1 s2

    intersectSC (UDS s1) (UDS s2)
        = UDS $ DS.intersection s1 s2

    intersectDisplSC _disp
        = intersectSC

    intersectFuzzySC _lb _ub
        = intersectSC

toUnScoredDocs :: Occurrences -> UnScoredDocs
toUnScoredDocs os
    = UDS . DS.fromList . DM.keys $ os

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
        = SCO (s1 + s2) (intersectOccurrences Pos.union d1 d2)

    intersectDisplSC disp (SCO s1 d1) (SCO s2 d2)
        = SCO (s1 + s2) (intersectOccurrences (Pos.intersectionWithDispl disp) d1 d2)

    intersectFuzzySC lb ub (SCO s1 d1) (SCO s2 d2)
        = SCO (s1 + s2) (intersectOccurrences (Pos.intersectionWithIntervall lb ub) d1 d2)

-- ------------------------------------------------------------
{-- not yet used,

-- the @ScoredRawDocs@ deals with lists of words,
-- and that suits better for completion search

-- The(?) result type for searching phrases and context search,
-- every word hit is associated with a score and a DocIdMap containing
-- the docs and position.
--
-- The positions are neccessary in phrase and context search,
-- afterwards the positions can be accumulated into a score,
-- e.g. by counting the occurences
--
-- A RawResult can easily be converted into this type

newtype ScoredWordOccs
    = SCWO (Map Word ScoredOccs)
      deriving (Show)

instance Monoid ScoredWordOccs where
    mempty
        = SCWO M.empty
    mappend (SCWO m1) (SCWO m2)
        = SCWO $ M.unionWith (<>) m1 m2

instance ScoredResult ScoredWordOccs where
    boost b (SCWO m)
        = SCWO $ M.map (boost b) m

    nullSC (SCWO m)
        = M.null m

    differenceSC (SCWO m1) (SCWO m2)
        = scwo $ M.map diff m1
          where
            diff :: ScoredOccs -> ScoredOccs
            diff so = M.foldl differenceSC so m2

    intersectSC
        = intersectSCWO intersectSC

    intersectDisplSC d
        = intersectSCWO $ intersectDisplSC d

    intersectFuzzySC lb ub
        = intersectSCWO $ intersectFuzzySC lb ub

intersectSCWO :: (ScoredOccs -> ScoredOccs -> ScoredOccs)
              -> ScoredWordOccs -> ScoredWordOccs -> ScoredWordOccs
intersectSCWO interSCO (SCWO m1) (SCWO m2)
        = scwo $ M.map intersect m1
          where
            intersect s0
                = M.foldl acc mempty m2
                  where
                    acc r s1 = r <> (interSCO s0 s1)

scwo :: Map Word ScoredOccs -> ScoredWordOccs
scwo = SCWO . M.filter (not . nullSC)
-- -}
-- ------------------------------------------------------------

-- A result type for searching phrases and context search,
-- every word hit or word sequence hit associated with a score and a DocIdMap containing
-- the docs and position.
--
-- The positions are neccessary in phrase and context search,
-- afterwards the positions can be accumulated into a score,
-- e.g. by counting the occurences
--
-- A RawResult can easily be converted into this type

newtype ScoredRawDocs
    = SRD [([Word], ScoredOccs)]
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
        = srd $ [(ws1, SCO sc1 (diff occ1 xs2)) | (ws1, SCO sc1 occ1) <- xs1]
          where
            diff occ xs
                = L.foldl op occ xs
                  where
                    op occ' (_ws2, SCO _sc2 occ2)
                        = DM.difference occ' occ2

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

intersectSRD :: (ScoredOccs -> ScoredOccs -> ScoredOccs)
              -> ScoredRawDocs -> ScoredRawDocs -> ScoredRawDocs
intersectSRD interSRD (SRD xs1) (SRD xs2)
    = srd $ [ (ws1 ++ ws2, interSRD socc1 socc2)
            | (ws1, socc1) <- xs1
            , (ws2, socc2) <- xs2
            ]

-- | smart constructor for ScoredRawDoc removing empty entries

srd :: [([Word], ScoredOccs)] -> ScoredRawDocs
srd
    = SRD . L.filter (not . nullSC . snd)

-- ------------------------------------------------------------

newtype ScoredCx a
    = SCX (Map Context a)
      deriving (Show)

-- type ScoredCxRawDocs
--    = ScoredCx ScoredRawDocs

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
-- conversions from RawResult into scored results

type CxRawResults = [(Context, RawResult)]

fromCxRawResults ::  (Word -> Score) -> CxRawResults -> ScoredCx ScoredRawDocs
fromCxRawResults sf crs
    = -- traceShow ("fromCxRawResults:"::String, crs, res) $
      res
      where
        res = mconcat $ L.map (uncurry $ fromRawResult sf) crs

fromRawResult :: (Word -> Score) -> Context -> RawResult -> ScoredCx ScoredRawDocs
fromRawResult sf cx rr
    = SCX $ M.singleton cx sr
      where
        sr = SRD $ L.map toScored rr
            where
              toScored (w, occ)
                  = ([w], SCO (sf w) occ)

limitCxRawResults :: Int -> CxRawResults -> CxRawResults
limitCxRawResults mx
    = L.map (second $ limitRawResult mx)

limitRawResult :: Int -> RawResult -> RawResult
limitRawResult maxDocs rs
    | maxDocs <= 0 = rs
    | otherwise = takeDocs maxDocs rs
    where
      takeDocs _  xs@[]
          = xs
      takeDocs _  xs@[_]
          = xs
      takeDocs mx (r@(_w, occ) : xs)
          = case DM.sizeWithLimit mx occ of
              Nothing -> [r]
              Just s  -> let mx' = mx - s in
                         if mx' <= 0
                            then [r]
                            else r : takeDocs mx' xs

-- | convert a set of scored doc ids into a list of documents containing the score
--
-- This list may be further sorted by score, partitioned into pages or ...

toDocsResult :: (Applicative m, Monad m, DocTable dt) =>
                dt -> ScoredDocs -> m [Document]
toDocsResult dt (SDS m)
    = mapM toDoc (DM.toList m)
      where
        toDoc (did, sc)
            = (toD . fromJust) <$> Dt.lookup did dt
            where
              toD d'
                  = d {score = score d * sc}
                  where
                    d = unwrap d'

sortByScore :: [Document] -> [Document]
sortByScore = L.sortBy (compare `on` score)

getResultPage :: Int -> Int -> [a] -> [a]
getResultPage start len
    = take len . drop start

-- ------------------------------------------------------------
--
-- aggregating (raw) results for various types of scored results

class Aggregate a b where
    aggregate :: a -> b

-- | allow no aggregation

instance Aggregate a a where
    aggregate = id

-- | aggregate scored docs to a single score by summing up the scores and throw away the DocIds

instance Aggregate ScoredDocs Score where
    aggregate (SDS m)
        = DM.foldr (<>) defScore m


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
    aggregate (SCO sc occ)
        = toUnScoredDocs occ

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
-- ------------------------------------------------------------

-- | The intermediate result used during query processing.

type Intermediate         = DocIdMap IntermediateContexts
type IntermediateContexts = (Map Context IntermediateWords, Boost)
type IntermediateWords    = Map Word (WordInfo, Positions)

-- ------------------------------------------------------------

-- | Create an empty intermediate result.
empty :: Intermediate
empty = DM.empty

-- | Check if the intermediate result is empty.
null :: Intermediate -> Bool
null = DM.null

-- | Returns the number of documents in the intermediate result.
size :: Intermediate -> Int
size = DM.size

-- | Merges a bunch of intermediate results into one intermediate result by unioning them.
unions :: [Intermediate] -> Intermediate
unions = L.foldl' union empty

-- | Intersect two sets of intermediate results.
intersection :: Intermediate -> Intermediate -> Intermediate
intersection = DM.intersectionWith combineContexts

{-
-- TODO: make this safe and efficient
-- foldl is inefficient because the neutral element of the intersection is >everything<

intersections1 :: [Intermediate] -> Intermediate
intersections1 = L.foldl1' intersection

-- TODO: same as for 'intersections1' but this is not commutative

differences1 :: [Intermediate] -> Intermediate
differences1 = L.foldl1' difference
-}

-- | Union two sets of intermediate results.
--   Can be used on \"query intermediates\".
--
-- /Note/: See 'merge' for a similar function.

union :: Intermediate -> Intermediate -> Intermediate
union = DM.unionWith combineContexts

-- | Merge two sets of intermediate results.
--   Search term should be the same.
--   Can be used on \"context intermediates\".
--
-- /Note/: See 'union' for a similar function.

merge :: Intermediate -> Intermediate -> Intermediate
merge = DM.unionWith mergeContexts

-- | Merges a bunch of intermediate results into one intermediate result by merging them.

merges :: [Intermediate] -> Intermediate
merges = L.foldl' merge empty

-- | Subtract two sets of intermediate results.

difference :: Intermediate -> Intermediate -> Intermediate
difference = DM.difference

-- | Create an intermediate result from a list of words and their occurrences.
--
-- The first arg is the phrase searched for split into its parts
-- all these parts are stored in the WordInfo as term
--
-- Beware! This is extremly optimized and will not work for merging arbitrary intermediate results!
-- Based on resultByDocument from Hunt.Common.RawResult
--
-- merge of list with 'head' because second argument is always a singleton
-- otherwise >> (flip $ (:) . head) [1,2] [3,4] == [3,1,2]

fromList :: Schema -> [Word] -> Context -> RawResult -> Intermediate
fromList sc ts c os
    = DM.map transform                            -- ::   DocIdMap IntermediateContexts
      $ DM.unionsWith (flip $ (:) . head)         -- ::   DocIdMap [(Word, (WordInfo, Positions))]
      $ map insertWords os                        -- :: [ DocIdMap [(Word, (WordInfo, Positions))] ]
    where
      -- O(size o)
      insertWords :: (Word, Occurrences) -> DocIdMap [(Word, (WordInfo, Positions))]
      insertWords (w, o)
          = DM.map toWordInfo o
            where
              toWordInfo o' = [(w, (WordInfo ts 0.0 , o'))] -- singleton list

      -- O(w*log w)
      transform :: [(Word, (WordInfo, Positions))] -> IntermediateContexts
      transform wl
          = ( M.singleton c (M.fromList wl)
            , weight
            )
      weight
          = fromMaybe defScore (cxWeight <$> M.lookup c sc)

-- XXX: optimize if necessary, see comments below
-- | Create an intermediate result from a list of words and their occurrences
--   with their associated context.

fromListCxs :: Schema -> [Word] -> [(Context, RawResult)] -> Intermediate
fromListCxs sc ts rs = merges $ map (uncurry (fromList sc ts)) rs

-- | Convert to a @Result@ by generating the 'WordHits' structure.
toResult :: (Applicative m, Monad m, DocTable dt, e ~ Dt.DValue dt) =>
            dt -> Intermediate -> m (Result e)
toResult dt im = do
    dh <- createDocHits dt im
    return $ Result dh (createWordHits im)


-- XXX: IntMap.size is O(n) :(
-- | Union 'Intermediate's until a certain number of documents is reached/surpassed.

unionsDocLimited :: Int -> [Intermediate] -> Intermediate
unionsDocLimited n = takeOne ((>= n) . size) . scanl union empty
  where
  takeOne b (x:xs) = if P.null xs || b x then x else takeOne b xs
  takeOne _ _      = error "takeOne with empty list"


-- | Create the doc hits structure from an intermediate result.
createDocHits :: (Applicative m, Monad m, DocTable dt, e ~ Dt.DValue dt) =>
                 dt -> Intermediate -> m (DocHits e)
createDocHits dt
    = DM.traverseWithKey transformDocs
    where
      transformDocs did (ic, db)
          = let doc   = fromMaybe dummy <$> (Dt.lookup did dt)
                dummy = wrap emptyDocument
            in (\doc' -> (DocInfo doc' db 0.0, M.map (M.map snd) ic)) <$> doc

-- | Create the word hits structure from an intermediate result.
--
-- the schema is used for the context weights
createWordHits :: Intermediate -> WordHits
createWordHits
    = DM.foldrWithKey transformDoc M.empty
    where
      -- XXX: boosting not used in wordhits
      transformDoc d (ic, _db) wh
          = M.foldrWithKey transformContext wh ic
          where
            transformContext c iw wh'
                = M.foldrWithKey insertWord wh' iw
                where
                  insertWord w (wi, pos) wh''
                      = if terms wi == [""]
                        then wh''
                        else M.insertWith (<>)
                             w
                             (WIH wi wh3)
                             wh''
                      where
                        wh3 = M.singleton c (DM.singleton d (toScore pos))
                            where
                              toScore = mkScore . fromIntegral . Pos.size

-- XXX: 'combineContexts' is used in 'union' and 'intersection'.
--      maybe it should include the merge op as a parameter.
--      there is a difference in merging "query intermediates" and "context intermediates".
--        docboosts merge:
--          - on context merge: should be always the same since the query introduces it
--          - on query merge: default merge
--      merging "context intermediates" seems inefficient - maybe not because of hedge-union?

-- XXX: db merge is skewed on a context merge - include merge op?
-- | Combine two tuples with score and context hits.

combineContexts :: IntermediateContexts -> IntermediateContexts -> IntermediateContexts
combineContexts (ic1,db1) (ic2,db2)
    = (M.unionWith (M.unionWith merge') ic1 ic2, db1 * db2)
    where
      merge' (i1, p1) (i2, p2)
          = ( i1 <> i2
            , Pos.union p1 p2
            )

mergeContexts :: IntermediateContexts -> IntermediateContexts -> IntermediateContexts
mergeContexts cx1 (ic2,_db2)
    = combineContexts cx1 (ic2, defScore)


-- XXX: IntMap.size is O(n) :(
-- | Merge 'Intermediate's until a certain number of documents is reached/surpassed.

mergesDocLimited :: Int -> [Intermediate] -> Intermediate
mergesDocLimited n = takeOne ((>= n) . size) . scanl merge empty
  where
  takeOne b (x:xs) = if P.null xs || b x then x else takeOne b xs
  takeOne _ _      = error "takeOne with empty list"

-- ------------------------------------------------------------
