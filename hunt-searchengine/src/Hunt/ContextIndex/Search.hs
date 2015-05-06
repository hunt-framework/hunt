{-# LANGUAGE Rank2Types #-}
module Hunt.ContextIndex.Search where

import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Scoring.Score (Score)
import           Hunt.Scoring.SearchResult

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)


-- TODO: find a better way, one which doesn't involve as much work

filterDocs :: DocIdSet -> [(a, SearchResult)] -> [(a, SearchResult)]
filterDocs dIds xs
  | DocIdSet.null dIds = xs
  | otherwise
    = filter (not . srNull . snd) . fmap (second (srDiffDocs dIds)) $ xs

filterDocs1 :: DocIdSet -> [(a, (b, SearchResult))] -> [(a, (b, SearchResult))]
filterDocs1 dIds xs
  | DocIdSet.null dIds = xs
  | otherwise
    = filter (not . srNull . snd . snd) . fmap (second (second (srDiffDocs dIds))) $ xs

searchWithCx :: Par.MonadParallel m => TextSearchOp -> Context ->
                Text -> ContextIndex dt -> m [(Text, SearchResult)]
searchWithCx op cx w ix
  = lookupIndex cx ix filterDocs  merge (Ix.searchM op w)

searchWithCxSc :: Par.MonadParallel m => TextSearchOp -> Context ->
                  Text -> ContextIndex dt -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ix
  = lookupIndex cx ix filterDocs1 merge (Ix.searchMSc op w)

lookupRangeCx :: Par.MonadParallel m => Context -> Text ->
                 Text -> ContextIndex dt -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 ix
  = lookupIndex c ix  filterDocs merge (Ix.lookupRangeM k1 k2)

lookupRangeCxSc :: Par.MonadParallel  m => Context -> Text ->
                   Text -> ContextIndex dt -> m  [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ix
  = lookupIndex c ix filterDocs1 merge (Ix.lookupRangeMSc k1 k2)

lookupAllWithCx :: Par.MonadParallel m => Context -> ContextIndex dt -> m  [(Text, SearchResult)]
lookupAllWithCx c ix
  = lookupIndex c ix filterDocs merge (Ix.toListM)

lookupIndex :: (Par.MonadParallel m) =>
               Context -> ContextIndex dt ->
               (DocIdSet -> [r] -> [r]) ->
               ([[r]] -> [r]) ->
               (forall i . Ix.IndexImplCon i => i -> m [r]) ->
               m [r]
lookupIndex cx ixx filterDocs merge search
  = do rx <- Par.mapM (\sn ->
                        do
                          if Set.member cx (snDeletedContexts sn)
                            then return []
                            else do r <- lookupIndex' cx (snContextMap sn) search
                                    return (filterDocs (snDeletedDocs sn) r)
                      ) (dummy:ciSnapshots ixx)
       return (merge rx)
  where
    dummy = Snapshot { snId              = SnapshotId 0
                     , snDeletedDocs     = mempty
                     , snDeletedContexts = mempty
                     , snContextMap      = ciIndex ixx
                     }

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
