module Hunt.ContextIndex1 (
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

  , ContextIndex
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Delete
import           Hunt.ContextIndex.Insert
import           Hunt.ContextIndex.Search
import           Hunt.ContextIndex.Types
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

import qualified Data.Map.Strict as Map

insertContext :: Context -> Ix.IndexImpl -> ContextSchema
                 -> ContextIndex dt -> ContextIndex dt
insertContext cx ix schema ixx
  = ixx { ciIndex  = mapHead (insertContext' cx ix) (ciIndex ixx)
        , ciSchema = Map.insertWith (const id) cx schema (ciSchema ixx)
        }

insertContext' :: Context -> Ix.IndexImpl -> ContextMap -> ContextMap
insertContext' cx ix
  = mkContextMap . Map.insertWith (const id) cx ix . cxMap

deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciIndex  = mapHead (deleteContext' cx) (ciIndex ixx)
        , ciSchema = Map.delete cx (ciSchema ixx)
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
