module Hunt.ContextIndex (
    -- * Construction
    empty

    -- * Contexts and Schema
  , insertContext
  , deleteContext
  , contexts
  , contextsM
  , defaultContexts
  , hasContext
  , hasContextM
  , schema

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
  , commit

  ) where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Delete
import           Hunt.ContextIndex.Documents
import           Hunt.ContextIndex.Insert
import           Hunt.ContextIndex.Search
import           Hunt.ContextIndex.Segment
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

deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciSegments = mapIxs' (segmentDeleteContext cx) ixx
        , ciSchema   = Map.delete cx (ciSchema ixx)
        }

defaultContexts :: ContextIndex dt -> [Context]
defaultContexts
  = Map.keys . Map.filter cxDefault . ciSchema

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

-- | Flushes all dirty and not yet written `Segment`s to the index directory
--
commit :: (Binary dt, DocTable dt, MonadIO m) => FilePath -> ContextIndex dt -> m (ContextIndex dt)
commit dir ixx
  = do segments' <- mapM commitSeg (ciSegments ixx)
       return ixx { ciSegments   = segments'
                  , ciUncommited = mempty
                  }
  where
    isUnstaged s
      = Set.member (segId s) (ciUncommited ixx)

    commitSeg s
      = do when (isUnstaged s) $
             commitSegment dir s
           when (segIsDirty s) $
             commitDirtySegment dir s
           return s { segIsDirty = False }
