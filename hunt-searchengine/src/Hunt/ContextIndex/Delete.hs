module Hunt.ContextIndex.Delete(
    deleteDocsByURI
  , delete
  , delete'
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import           Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set


-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => Set URI -> ContextIndex dt -> m (ContextIndex dt)
deleteDocsByURI us ixx = do
  docIds <- liftM (DocIdSet.fromList . catMaybes)
            . mapM (flip DocTable.lookupByURI (ciDocs ixx))
            . Set.toList $ us
  delete docIds ixx

-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => DocIdSet -> ContextIndex dt -> m (ContextIndex dt)
delete dIds ixx
    | DocIdSet.null dIds = return ixx
    | otherwise
        = do  newIx <- delete' dIds ixx
              newDt <- DocTable.difference dIds (ciDocs newIx)
              return $ newIx { ciDocs = newDt }

delete' :: (Monad m, Par.MonadParallel m) => DocIdSet -> ContextIndex dt -> m (ContextIndex dt)
delete' dIds ixx
  = do ix' <- mapHeadM delIx (ciIndex ixx)
       return ixx { ciIndex = ix'
                  , ciDeleted = dIds `DocIdSet.union` (ciDeleted ixx)
                  }
    where
      delIx (ContextMap m)
        = do m' <- mapWithKeyMP adjust' m
             return (mkContextMap m')

      adjust' _ (Ix.IndexImpl ix)
        = do ix' <- Ix.deleteDocsM dIds ix
             return (Ix.mkIndex ix')

-- TODO: duplicate
mapWithKeyMP :: (Par.MonadParallel m, Ord k) => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyMP f m =
  (Par.mapM (\(k, a) ->
              do b <- f k a
                 return (k, b)
            ) $ Map.toAscList m) >>=
    return . Map.fromAscList
