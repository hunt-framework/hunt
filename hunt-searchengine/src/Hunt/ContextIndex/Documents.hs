module Hunt.ContextIndex.Documents where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.Common.Document (Document)
import qualified Hunt.Common.Document as Document
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import           Data.Maybe

reduceMaybes' :: [Maybe a] -> Maybe a
reduceMaybes' = listToMaybe . catMaybes

lookupDocumentByURI :: (DocTable dt, Par.MonadParallel m) => URI -> ContextIndex dt -> m (Maybe DocId)
lookupDocumentByURI uri ixx
  = do dIds <- mapIxsP docs ixx
       return (reduceMaybes' dIds)
    where
      docs seg
        = do di  <- DocTable.lookupByURI uri (segDocs seg)
             case di of
              Just dId | not (docsMember dId seg) -> return (Just dId)
              _                                   -> return Nothing

lookupDocument :: (DocTable dt, Par.MonadParallel m) => ContextIndex dt -> DocId -> m (Maybe Document)
lookupDocument ixx dId
  = do docs <- mapIxsP doc ixx
       return (fmap Document.unwrap (reduceMaybes' docs))
  where
    doc seg = if docsMember dId seg
              then return Nothing
              else DocTable.lookup dId (segDocs seg)

selectDocuments :: (Applicative m, Par.MonadParallel m, DocTable dt) => ContextIndex dt -> DocIdSet -> m dt
selectDocuments ixx dIds
  = do dtx <- mapIxsP docs ixx
       foldM DocTable.union DocTable.empty dtx
  where
    docs seg
      = do dt <- DocTable.restrict dIds (segDocs seg)
           DocTable.difference (segDeletedDocs seg) dt

docsMember dId seg = DocIdSet.member dId (segDeletedDocs seg)
