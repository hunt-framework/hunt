module Hunt.ContextIndex.Documents where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap (DocIdMap)
import qualified Hunt.Common.DocIdMap as DocIdMap
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.Common.Document (Document)
import qualified Hunt.Common.Document as Document
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable, DValue)
import qualified Hunt.DocTable as DocTable

import           Control.Applicative
import           Control.Monad
import qualified Data.List as List
import           Data.Maybe

lookupDocumentByURI :: (DocTable dt, Monad m) => URI -> ContextIndex dt -> m (Maybe DocId)
lookupDocumentByURI uri ixx
  = do dIds <- mapIxs (\seg -> do di  <- DocTable.lookupByURI uri (segDocs seg)
                                  case di of
                                   Nothing -> return Nothing
                                   Just dId | DocIdSet.member dId (segDeletedDocs seg)
                                              -> return Nothing
                                            | otherwise
                                              -> return (Just dId)
                   ) ixx
       case catMaybes dIds of
        []  -> return Nothing
        [x] -> return (Just x)


lookupDocument :: (DocTable dt, Monad m) => ContextIndex dt -> DocId -> m (Maybe Document)
lookupDocument ixx dId
  = do docs <- mapIxs (\seg -> if DocIdSet.member dId (segDeletedDocs seg)
                               then return Nothing
                               else DocTable.lookup dId (segDocs seg)
                      ) ixx

       case catMaybes docs of
        []  -> return Nothing
        [x] -> return (Just (Document.unwrap x))

selectDocuments :: (Applicative m, Monad m, DocTable dt) => ContextIndex dt -> DocIdSet -> m dt
selectDocuments ixx dIds
  = do dtx <- mapIxs (\seg -> do dt <- DocTable.restrict dIds (segDocs seg)
                                 DocTable.difference (segDeletedDocs seg) dt
                     ) ixx
       foldM DocTable.union DocTable.empty dtx
