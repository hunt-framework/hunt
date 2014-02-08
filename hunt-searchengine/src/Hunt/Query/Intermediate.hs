{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Hunt.Query.Intermediate
(
  -- * The intermediate result type.
  Intermediate
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
  , intersections1
  , differences1

  -- * Conversion
  , fromList
  , fromListCxs
  , toResult
)
where

import           Prelude                hiding (null)
import qualified Prelude                as P

import           Control.Applicative    hiding (empty)

import           Data.Maybe

import qualified Data.List              as L

import           Data.Map               (Map)
import qualified Data.Map               as M

import           Hunt.Query.Result      hiding (null)

import           Hunt.Common
import qualified Hunt.Common.DocIdMap   as DM
import           Hunt.Common.Document   (DocumentWrapper (..))
import qualified Hunt.Common.Positions  as Pos

import           Hunt.DocTable.DocTable (DocTable)
import qualified Hunt.DocTable.DocTable as Dt

-- ----------------------------------------------------------------------------

-- | The intermediate result used during query processing.

type Intermediate               = DocIdMap IntermediateContexts
type IntermediateContexts       = (Map Context IntermediateWords, Boost)
type IntermediateWords          = Map Word (WordInfo, Positions)

-- ----------------------------------------------------------------------------

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

-- TODO: make this safe and efficient
-- foldl is inefficient because the neutral element of the intersection is >everything<
intersections1 :: [Intermediate] -> Intermediate
intersections1 = L.foldl1' intersection

-- TODO: same as for 'intersections1' but this is not commutative
differences1 :: [Intermediate] -> Intermediate
differences1 = L.foldl1' difference

-- | Union two sets of intermediate results.
--   Can be used on "query intermediates".
-- /NOTE/: See 'merge' for a similar function.
union :: Intermediate -> Intermediate -> Intermediate
union = DM.unionWith combineContexts

-- | Merge two sets of intermediate results.
--   Search term should be the same.
--   Can be used on "context intermediates".
-- /NOTE/: See 'union' for a similar function.
merge :: Intermediate -> Intermediate -> Intermediate
merge = DM.unionWith mergeContexts

-- | Merges a bunch of intermediate results into one intermediate result by merging them.
merges :: [Intermediate] -> Intermediate
merges = L.foldl' merge empty

-- | Subtract two sets of intermediate results.
difference :: Intermediate -> Intermediate -> Intermediate
difference = DM.difference

-- | Create an intermediate result from a list of words and their occurrences.
fromList :: Word -> Context -> RawResult -> Intermediate
-- Beware! This is extremly optimized and will not work for merging arbitrary intermediate results!
-- Based on resultByDocument from Hunt.Index.Common.RawResult
fromList t c os
  = DM.map transform .
      DM.unionsWith (flip $ (:) . head) $ -- merge of list with 'head' because second argument is always a singleton
                                          -- otherwise >> (flip $ (:) . head) [1,2] [3,4] == [3,1,2]
        map insertWords os
  where
  -- O(o)
  insertWords :: (Word, Occurrences) -> DocIdMap [(Word, (WordInfo, Positions))]
  insertWords (w, o) = DM.map (\p -> [(w, (WordInfo [t] 0.0 , p))]) o -- singleton
  -- O(w*log w)
  transform :: [(Word, (WordInfo, Positions))] -> IntermediateContexts
  transform wl       = (M.singleton c (M.fromList wl), []) -- XXX: empty docboost

-- XXX: optimize if necessary, see comments below
fromListCxs :: Word -> [(Context, RawResult)] -> Intermediate
fromListCxs t rs = merges $ map (uncurry (fromList t)) rs

-- | Convert to a @Result@ by generating the 'WordHits' structure.
toResult :: (Applicative m, Monad m, DocTable d, e ~ Dt.DValue d) =>
            d -> Intermediate -> m (Result e)
toResult d im = do
    dh <- createDocHits d im
    return $ Result dh (createWordHits im)


-- XXX: IntMap.size is O(n) :(
-- | Union 'Intermediate's until a certain number of documents is reached/surpassed.
unionsDocLimited :: Int -> [Intermediate] -> Intermediate
unionsDocLimited n = takeOne ((>= n) . size) . scanl union empty
  where
  takeOne b (x:xs) = if P.null xs || b x then x else takeOne b xs
  takeOne _ _      = error "takeOne with empty list"


-- | Create the doc hits structure from an intermediate result.
createDocHits :: (Applicative m, Monad m, DocTable d, e ~ Dt.DValue d) =>
                 d -> Intermediate -> m (DocHits e)
createDocHits d = DM.traverseWithKey transformDocs
  where
  transformDocs did (ic,db)
    = let doc   = fromMaybe dummy <$> (Dt.lookup d did)
          dummy = wrap (Document "" M.empty)
      in (\doc' -> (DocInfo doc' db 0.0, M.map (M.map snd) ic)) <$> doc

-- | Create the word hits structure from an intermediate result.
createWordHits :: Intermediate -> WordHits
createWordHits = DM.foldrWithKey transformDoc M.empty
  where
  -- XXX: boosting not used in wordhits
  transformDoc d (ic, _db) wh = M.foldrWithKey transformContext wh ic
    where
    transformContext c iw wh' = M.foldrWithKey insertWord wh' iw
      where
      insertWord w (wi, pos) wh''
        = if terms wi == [""]
          then wh''
          else M.insertWith combineWordHits
                  w
                  (wi, M.singleton c (DM.singleton d pos))
                  wh''

-- | Combine two tuples with score and context hits.
combineWordHits :: (WordInfo, WordContextHits) -> (WordInfo, WordContextHits) -> (WordInfo, WordContextHits)
combineWordHits (i1, c1) (i2, c2)
  = ( mergeWordInfo i1 i2
    , M.unionWith (DM.unionWith Pos.union) c1 c2
    )

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
combineContexts (ic1,db1) (ic2,db2) = (M.unionWith (M.unionWith merge') ic1 ic2, db1 ++ db2) -- XXX: db merge
  where
  merge' (i1, p1) (i2, p2)
    = ( combineWordInfo i1 i2
      , Pos.union p1 p2
      )

-- XXX: concat on terms - extend with info about the merge operation and maybe use a set?
-- | Combine two word informations.
combineWordInfo :: WordInfo -> WordInfo -> WordInfo
combineWordInfo (WordInfo t1 s1) (WordInfo t2 s2)
  = WordInfo (t1 ++ t2) (combineScore s1 s2)

-- XXX: oblivious to the merge operation (union, intersection).
-- | Combine two scores (just average between them).
combineScore :: Score -> Score -> Score
combineScore s1 s2 = (s1 + s2) / 2.0

-- ----------------------------------------------------------------------------

-- XXX: code duplication - maybe branch in unionContexts with op

mergeContexts :: IntermediateContexts -> IntermediateContexts -> IntermediateContexts
mergeContexts (ic1,db1) (ic2,_db2) = (M.unionWith (M.unionWith merge') ic1 ic2, db1)
-- doc boosts are not context-sensitive and should be the same for Intermediates that are merged
  where
  merge' (i1, p1) (i2, p2)
    = ( mergeWordInfo i1 i2
      , Pos.union p1 p2
      )

mergeWordInfo :: WordInfo -> WordInfo -> WordInfo
mergeWordInfo (WordInfo t1 s1) (WordInfo _t2 s2)
  = WordInfo t1 (mergeScore s1 s2) -- term should be the same

-- XXX: oblivious to the merge operation (union, intersection).
-- | Combine two scores (just average between them).
mergeScore :: Score -> Score -> Score
mergeScore s1 s2 = (s1 + s2) / 2.0 -- TODO: reasonable score merge op?

-- XXX: IntMap.size is O(n) :(
-- | Merge 'Intermediate's until a certain number of documents is reached/surpassed.
mergesDocLimited :: Int -> [Intermediate] -> Intermediate
mergesDocLimited n = takeOne ((>= n) . size) . scanl merge empty
  where
  takeOne b (x:xs) = if P.null xs || b x then x else takeOne b xs
  takeOne _ _      = error "takeOne with empty list"

-- ----------------------------------------------------------------------------
