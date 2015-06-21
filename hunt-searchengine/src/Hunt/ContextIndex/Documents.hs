module Hunt.ContextIndex.Documents where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.Common.Document (Document)
import qualified Hunt.Common.Document as Document
import           Hunt.ContextIndex.Types
import           Hunt.ContextIndex.Segment
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import           Data.Maybe

reduceMaybes' :: [Maybe a] -> Maybe a
reduceMaybes' = listToMaybe . catMaybes

lookupDocumentByURI :: (DocTable dt, Par.MonadParallel m)
                    => URI
                    -> ContextIndex dt
                    -> m (Maybe DocId)
lookupDocumentByURI uri ixx
  = do dIds <- mapIxsP (\seg -> do di <- DocTable.lookupByURI uri (segDocs seg)
                                   return (do dId <- di
                                              guard (not (isDeleted dId seg))
                                              return dId)
                       ) ixx
       return (reduceMaybes' dIds)

lookupDocument :: (DocTable dt, Par.MonadParallel m)
               => ContextIndex dt
               -> DocId
               -> m (Maybe Document)
lookupDocument ixx dId
  = do docs <- mapIxsP doc ixx
       return (fmap Document.unwrap (reduceMaybes' docs))
  where
    doc seg
      = if isDeleted dId seg
        then return Nothing
        else DocTable.lookup dId (segDocs seg)

selectDocuments :: (Applicative m, Par.MonadParallel m, DocTable dt)
                => ContextIndex dt
                -> DocIdSet
                -> m dt
selectDocuments ixx dIds
  = do dtx <- mapIxsP docs ixx
       foldM DocTable.union DocTable.empty dtx
  where
    docs seg
      = do dt <- DocTable.restrict dIds (segDocs seg)
           DocTable.difference (segDeletedDocs seg) dt

isDeleted :: DocId -> Segment dt -> Bool
isDeleted dId
  = DocIdSet.member dId . segDeletedDocs
