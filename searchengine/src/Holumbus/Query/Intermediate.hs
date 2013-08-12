-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Intermediate
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  The data type for intermediate results occuring during query processing.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS #-}

module Holumbus.Query.Intermediate
(
  -- * The intermediate result type.
  Intermediate

  -- * Construction
  , empty

  -- * Query
  , null
  , size

  -- * Combine
  , union
  , difference
  , intersection
  , unions

  -- * Conversion
  , fromList
  , toResult
)
where

import           Prelude                        hiding (null)

import           Data.Maybe

import qualified Data.List                      as L

import           Data.Map                       (Map)
import qualified Data.Map                       as M

import           Holumbus.Query.Result          hiding (null)

import           Holumbus.Index.Common          hiding (empty, null, size)
import qualified Holumbus.Index.Common.DocIdMap as DM

import           Holumbus.DocTable.DocTable     (DocTable)
import qualified Holumbus.DocTable.DocTable     as Dt

-- ----------------------------------------------------------------------------

-- | The intermediate result used during query processing.

type Intermediate               = DocIdMap IntermediateContexts
type IntermediateContexts       = Map Context IntermediateWords
type IntermediateWords          = Map Word (WordInfo, Positions)

-- ----------------------------------------------------------------------------

-- | Create an empty intermediate result.
empty                           :: Intermediate
empty                           = DM.empty

-- | Check if the intermediate result is empty.
null                            :: Intermediate -> Bool
null                            = DM.null

-- | Returns the number of documents in the intermediate result.
size                            :: Intermediate -> Int
size                            = DM.size

-- | Merges a bunch of intermediate results into one intermediate result by unioning them.
unions                          :: [Intermediate] -> Intermediate
unions                          = L.foldl' union empty

-- | Intersect two sets of intermediate results.
intersection                    :: Intermediate -> Intermediate -> Intermediate
intersection                    = DM.intersectionWith combineContexts

-- | Union two sets of intermediate results.
union                           :: Intermediate -> Intermediate -> Intermediate
union                           = DM.unionWith combineContexts

-- | Substract two sets of intermediate results.
difference                      :: Intermediate -> Intermediate -> Intermediate
difference                      = DM.difference

-- | Create an intermediate result from a list of words and their occurrences.
fromList                        :: Word -> Context -> RawResult -> Intermediate
-- Beware! This is extremly optimized and will not work for merging arbitrary intermediate results!
-- Based on resultByDocument from Holumbus.Index.Common

fromList t c os                 = DM.map transform $
                                  DM.unionsWith (flip $ (:) . head)
                                                     (map insertWords os)
  where
  insertWords (w, o)            = DM.map (\p -> [(w, (WordInfo [t] 0.0 , p))]) o
  transform w                   = M.singleton c (M.fromList w)

-- | Convert to a @Result@ by generating the 'WordHits' structure.
toResult                        :: DocTable d Document -> Intermediate -> Result
toResult d im                   = Result (createDocHits d im) (createWordHits im)

-- | Create the doc hits structure from an intermediate result.
createDocHits                   :: DocTable d Document -> Intermediate -> DocHits
createDocHits d                 = DM.mapWithKey transformDocs
  where
  transformDocs did ic          = let doc = fromMaybe (Document "" M.empty) (Dt.lookupById d did) in
                                  (DocInfo doc 0.0, M.map (M.map snd) ic)

-- | Create the word hits structure from an intermediate result.
createWordHits                  :: Intermediate -> WordHits
createWordHits                  = DM.foldrWithKey transformDoc M.empty
  where
  transformDoc d ic wh          = M.foldrWithKey transformContext wh ic
    where
    transformContext c iw wh'   = M.foldrWithKey insertWord wh' iw
      where
      insertWord w (wi, pos) wh''
                                = if terms wi == [""]
                                  then wh''
                                  else M.insertWith combineWordHits
                                           w
                                           (wi, M.singleton c (DM.singleton d pos))
                                           wh''

-- | Combine two tuples with score and context hits.
combineWordHits                 :: (WordInfo, WordContextHits) ->
                                   (WordInfo, WordContextHits) -> (WordInfo, WordContextHits)
combineWordHits (i1, c1) (i2, c2)
                                = ( combineWordInfo i1 i2
                                  , M.unionWith (DM.unionWith unionPos) c1 c2
                                  )

-- | Combine two tuples with score and context hits.
combineContexts                 :: IntermediateContexts -> IntermediateContexts -> IntermediateContexts
combineContexts                 = M.unionWith (M.unionWith merge')
  where
  merge' (i1, p1) (i2, p2)      = ( combineWordInfo i1 i2
                                  , unionPos p1 p2
                                  )

-- | Combine two word informations.
combineWordInfo                 :: WordInfo -> WordInfo -> WordInfo
combineWordInfo (WordInfo t1 s1) (WordInfo t2 s2)
                                = WordInfo (t1 ++ t2) (combineScore s1 s2)

-- | Combine two scores (just average between them).
combineScore                    :: Score -> Score -> Score
combineScore s1 s2              = (s1 + s2) / 2.0

-- ----------------------------------------------------------------------------
