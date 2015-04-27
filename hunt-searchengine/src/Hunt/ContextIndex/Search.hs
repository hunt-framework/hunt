{-# LANGUAGE Rank2Types #-}
module Hunt.ContextIndex.Search where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Scoring.Score (Score)
import           Hunt.Scoring.SearchResult

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)

searchWithCx :: Par.MonadParallel m => TextSearchOp -> Context ->
                Text -> ContextIndex dt -> m [(Text, SearchResult)]
searchWithCx op cx w ix
  = lookupIndex cx ix merge (Ix.searchM op w)

searchWithCxSc :: Par.MonadParallel m => TextSearchOp -> Context ->
                  Text -> ContextIndex dt -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ix
  = lookupIndex cx ix merge (Ix.searchMSc op w)

lookupRangeCx :: Par.MonadParallel m => Context -> Text ->
                 Text -> ContextIndex dt -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeM k1 k2)

lookupRangeCxSc :: Par.MonadParallel  m => Context -> Text ->
                   Text -> ContextIndex dt -> m [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeMSc k1 k2)

lookupIndex :: (Par.MonadParallel m) =>
               Context -> ContextIndex dt ->
               ([[r]] -> [r]) ->
               (forall i . Ix.IndexImplCon i => i -> m [r]) ->
               m [r]
lookupIndex cx ixx merge search
  = do rx <- Par.mapM (\cm ->
                        lookupIndex' cx cm search
                      ) (ciIndex ixx)
       return (merge rx)

lookupIndex' :: Monad m =>
                Context -> ContextMap ->
                (forall i . Ix.IndexImplCon i => i -> m [r]) ->
                m [r]
lookupIndex' cx (ContextMap m) search
    = case Map.lookup cx m of
       Just (Ix.IndexImpl cm) -> search cm
       Nothing                -> return []

merge :: (Ord a, Monoid b) => [[(a, b)]] -> [(a, b)]
merge
  = Map.toList . Map.unionsWith mappend . fmap Map.fromList

-- | Is the document part of the index?
member :: (Monad m, Applicative m, DocTable dt)
       => URI -> ContextIndex dt -> m Bool
member u ixx = do
  mem <- DocTable.lookupByURI u (ciDocs ixx)
  return $ isJust mem
