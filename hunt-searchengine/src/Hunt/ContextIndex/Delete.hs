module Hunt.ContextIndex.Delete(
    deleteDocsByURI
  , delete
  , delete'
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Segment
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable

import           Control.Applicative
import qualified Control.Monad.Parallel as Par
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => Set URI -> ContextIndex dt -> m (ContextIndex dt)
deleteDocsByURI us ixx
  = do dIdsx <- mapIxsP docs ixx
       delete (mconcat dIdsx) ixx
  where
    docs seg
      = do raw <- mapM (\uri -> DocTable.lookupByURI uri (segDocs seg)) (Set.toList us)
           return (DocIdSet.fromList (catMaybes raw))

-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => DocIdSet
       -> ContextIndex dt
       -> m (ContextIndex dt)
delete dIds ixx
    | DocIdSet.null dIds = return ixx
    | otherwise          = delete' dIds ixx

delete' :: Par.MonadParallel m
        => DocIdSet
        -> ContextIndex dt
        -> m (ContextIndex dt)
delete' dIds ixx
  = do sx <- mapIxsP (return . segmentDeleteDocs dIds) ixx
       return ixx { ciSegments = sx }
