{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    -- * Merge specific
  , MergeDescr
  , MergeLock
  , MergePolicy(..)
  , ApplyMerge(..)
  , runMerge
  , tryMerge

  , commit

  , status
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Commit
import           Hunt.ContextIndex.Delete
import           Hunt.ContextIndex.Documents
import           Hunt.ContextIndex.Insert
import           Hunt.ContextIndex.Merge
import           Hunt.ContextIndex.Search
import           Hunt.ContextIndex.Segment hiding (delete)
import           Hunt.ContextIndex.Status
import           Hunt.ContextIndex.Types
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

import qualified Data.Map.Strict as Map

insertContext :: Context
              -> Ix.IndexImpl
              -> ContextSchema
              -> ContextIndex dt
              -> ContextIndex dt
insertContext cx _ix s ixx
  = ixx { ciSchema = Map.insertWith (const id) cx s (ciSchema ixx) }

deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciSegments = mapSegments (segmentDeleteContext cx) (ciSegments ixx)
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
