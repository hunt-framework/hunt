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
  = do dIds <- mapIxsP (\seg -> do di  <- DocTable.lookupByURI uri (segDocs seg)
                                   case di of
                                    Nothing -> return Nothing
                                    Just dId | DocIdSet.member dId (segDeletedDocs seg)
                                               -> return Nothing
                                             | otherwise
                                               -> return (Just dId)
                   ) ixx
       return (reduceMaybes' dIds)

lookupDocument :: (DocTable dt, Par.MonadParallel m) => ContextIndex dt -> DocId -> m (Maybe Document)
lookupDocument ixx dId
  = do docs <- mapIxsP (\seg -> if DocIdSet.member dId (segDeletedDocs seg)
                                 then return Nothing
                                 else DocTable.lookup dId (segDocs seg)
                      ) ixx

       return (fmap Document.unwrap (reduceMaybes' docs))

selectDocuments :: (Applicative m, Par.MonadParallel m, DocTable dt) => ContextIndex dt -> DocIdSet -> m dt
selectDocuments ixx dIds
  = do dtx <- mapIxsP (\seg -> do dt <- DocTable.restrict dIds (segDocs seg)
                                  DocTable.difference (segDeletedDocs seg) dt
                     ) ixx
       foldM DocTable.union DocTable.empty dtx
