{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Types where

import           Data.Binary
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import           Data.Word (Word32)


import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

newtype ContextMap
  = ContextMap { cxMap :: Map Context Ix.IndexImpl }
  deriving (Show)

data ContextIndex dt
  = ContextIndex { ciIndex     :: !ContextMap
                 , ciSnapshots :: ![Snapshot]
                 , ciSchema    :: !Schema
                 , ciDocs      :: !dt
                 }

newtype SnapshotId
  = SnapshotId { unSnapshotId :: Word32 }
    deriving (Enum, Eq, Ord, Show)

data Snapshot
  = Snapshot { snId              :: !SnapshotId
             , snDeletedDocs     :: !DocIdSet
             , snDeletedContexts :: !(Set Context)
             , snContextMap      :: !ContextMap
             }

instance Binary dt => Binary (ContextIndex dt) where
  get = undefined
  put = undefined

mkContextMap :: Map Context Ix.IndexImpl -> ContextMap
mkContextMap m = ContextMap $! m

empty :: DocTable dt => ContextIndex dt
empty
  = ContextIndex { ciIndex     = mkContextMap mempty
                 , ciSnapshots = mempty
                 , ciSchema    = mempty
                 , ciDocs      = DocTable.empty
                 }

newContextMap :: Schema -> ContextMap
newContextMap = ContextMap . Map.map (newIx . ctIxImpl . cxType)
  where
    newIx :: Ix.IndexImpl -> Ix.IndexImpl
    newIx (Ix.IndexImpl i) = Ix.mkIndex (Ix.empty `asTypeOf` i)
