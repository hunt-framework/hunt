module Hunt.ContextIndex.Snapshot where

import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Types
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

zero :: SnapshotId
zero = SnapshotId 0

snapshot :: ContextIndex dt -> (Snapshot, ContextIndex dt)
snapshot ixx = ixx' `seq` (head (ciSnapshots ixx'), ixx')
  where
    ixx' = snapshot' ixx

snapshotM :: Monad m => ContextIndex dt -> m (Snapshot, ContextIndex dt)
snapshotM = return . snapshot

snapshot' :: ContextIndex dt -> ContextIndex dt
snapshot' ixx = ixx { ciIndex     = cm
                    , ciSnapshots = sn : ciSnapshots ixx
                    }
  where
    sn
      = Snapshot { snId              = sid
                 , snDeletedDocs     = mempty
                 , snDeletedContexts = mempty
                 , snContextMap      = ciIndex ixx
                 }

    sid
      = case ciSnapshots ixx of
         (s:_) -> succ (snId s)
         _     -> succ zero

    cm
      = newContextMap (ciSchema ixx)

newContextMap :: Schema -> ContextMap
newContextMap = ContextMap . Map.map (newIx . ctIxImpl . cxType)
  where
    newIx :: Ix.IndexImpl -> Ix.IndexImpl
    newIx (Ix.IndexImpl i) = Ix.mkIndex (Ix.empty `asTypeOf` i)

snDiffDocs :: DocIdSet -> Snapshot -> Snapshot
snDiffDocs dIds sn
  = sn { snDeletedDocs = dIds `DocIdSet.union` snDeletedDocs sn }

snDiffContexts :: Set Context -> Snapshot -> Snapshot
snDiffContexts cxs sn
  = sn { snDeletedContexts = cxs `Set.union` snDeletedContexts sn }
