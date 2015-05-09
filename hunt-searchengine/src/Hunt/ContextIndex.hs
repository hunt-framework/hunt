module Hunt.ContextIndex (
    -- * Construction
    empty

    -- * Contexts and Schema
  , insertContext
  , deleteContext
  , contexts
  , contextsM
  , hasContext
  , hasContextM

    -- * Queries
  , lookupRangeCx
  , searchWithCx
  , searchWithCxSc
  , lookupRangeCxSc
  , lookupAllWithCx

    -- * Insert\/Delete Documents
  , insertList
                                 -- XXX: these functions should be internal
                                 -- we export them to be able to test them
                                 -- is there a bedder approach to achieve this?
  , createDocTableFromPartition  -- only used in tests
  , unionDocTables               -- only used in tests
  , modifyWithDescription
  , delete
  , deleteDocsByURI
  , member

    -- * Documents
  , lookupDocumentByURI
  , lookupDocument
  , selectDocuments

  , ContextIndex
  , schema
  , commit

  ) where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Delete
import           Hunt.ContextIndex.Documents
import           Hunt.ContextIndex.Insert
import           Hunt.ContextIndex.Search
import           Hunt.ContextIndex.Segment
import           Hunt.ContextIndex.Snapshot
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Data.Set as Set

insertContext :: Context -> Ix.IndexImpl -> ContextSchema
                 -> ContextIndex dt -> ContextIndex dt
insertContext cx ix schema ixx
  = ixx { ciSchema = Map.insertWith (const id) cx schema (ciSchema ixx) }

insertContext' :: Context -> Ix.IndexImpl -> ContextMap -> ContextMap
insertContext' cx ix
  = mkContextMap . Map.insertWith (const id) cx ix . cxMap

deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciSegments = mapIxs' (segmentDeleteContext cx) ixx
        , ciSchema   = Map.delete cx (ciSchema ixx)
        }

deleteContext' :: Context -> ContextMap -> ContextMap
deleteContext' cx
  = mkContextMap . Map.delete cx . cxMap

contexts :: ContextIndex dt -> [Context]
contexts
  = Map.keys . ciSchema

contextsM :: Monad m => ContextIndex dt -> m [Context]
contextsM
  = return . contexts

hasContext :: Context -> ContextIndex dt -> Bool
hasContext cx
  = Map.member cx . ciSchema

hasContextM :: Monad m => Context -> ContextIndex dt -> m Bool
hasContextM cx
  = return . hasContext cx

schema :: ContextIndex dt -> Schema
schema = ciSchema

commit :: (Binary dt, DocTable dt, MonadIO m) => FilePath -> ContextIndex dt -> m (ContextIndex dt)
commit dir ixx
  = do segments' <- mapM (\s -> do when (isUnstaged s) $ do
                                     commitSegment dir s
                                   when (segIsDirty s) $ do
                                     commitDirtySegment dir s
                                   return s { segIsDirty = False }
                         ) (ciSegments ixx)

       return ixx { ciSegments   = segments'
                  , ciUncommited = mempty
                  }
  where
    isUnstaged s = Set.member (segId s) (ciUncommited ixx)
