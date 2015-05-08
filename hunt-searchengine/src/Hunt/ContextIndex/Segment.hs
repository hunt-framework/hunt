module Hunt.ContextIndex.Segment where

import qualified Data.Set as Set
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Types
import qualified Hunt.DocTable as DocTable

import           Data.Monoid

segmentDeleteDocs :: DocIdSet -> Segment dt -> Segment dt
segmentDeleteDocs dIds seg
  = seg { segIsDirty     = True
        , segDeletedDocs = dIds `mappend` segDeletedDocs seg
        }

segmentDeleteContext :: Context -> Segment dt -> Segment dt
segmentDeleteContext cx seg
  = seg { segIsDirty    = True
        , segDeletedCxs = Set.insert cx (segDeletedCxs seg)
        }
