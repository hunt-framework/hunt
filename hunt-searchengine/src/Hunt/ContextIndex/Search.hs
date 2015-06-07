{-# LANGUAGE Rank2Types #-}
module Hunt.ContextIndex.Search where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Segment
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Scoring.Score (Score)
import           Hunt.Scoring.SearchResult

import           Control.Applicative
import qualified Control.Monad.Parallel as Par
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)

searchWithCx :: Par.MonadParallel m
             => TextSearchOp
             -> Context
             -> Text
             -> ContextIndex dt
             -> m [(Text, SearchResult)]
searchWithCx op cx w ix
  = lookupIndex cx ix merge (Ix.searchM op w)

searchWithCxSc :: Par.MonadParallel m
               => TextSearchOp
               -> Context
               -> Text
               -> ContextIndex dt
               -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ix
  = lookupIndex cx ix merge (Ix.searchMSc op w)

lookupRangeCx :: Par.MonadParallel m
              => Context
              -> Text
              -> Text
              -> ContextIndex dt
              -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeM k1 k2)

lookupRangeCxSc :: Par.MonadParallel m
                => Context
                -> Text
                -> Text
                -> ContextIndex dt
                -> m [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeMSc k1 k2)

lookupAllWithCx :: Par.MonadParallel m
                => Context
                -> ContextIndex dt
                -> m [(Text, SearchResult)]
lookupAllWithCx c ix
  = lookupIndex c ix merge (Ix.toListM)

lookupIndex :: (Par.MonadParallel m, Ix.HasSearchResult r)
            => Context
            -> ContextIndex dt
            -> ([[r]] -> [r])
            -> (forall i . Ix.IndexImplCon i => i -> m [r])
            -> m [r]
lookupIndex cx ixx mrg search
  = do rx <- mapIxsP (searchSegment cx search) ixx
       return (mrg rx)
{-# INLINE lookupIndex #-}

merge :: (Ord a, Monoid b) => [[(a, b)]] -> [(a, b)]
merge
  = Map.toList . Map.unionsWith mappend . fmap Map.fromList
{-# INLINE merge #-}

-- | Is the document part of the index?
member :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => URI
       -> ContextIndex dt
       -> m Bool
member u ixx = do
  mems <- mapIxsP (DocTable.lookupByURI u . segDocs) ixx
  return (List.any isJust mems)
