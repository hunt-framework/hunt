module Fox.Index.State where

import qualified Fox.Index.Segment as Segment
import qualified Fox.Schema as Schema
import qualified Fox.Types.Generation as Generation
import qualified Fox.Types.SegmentMap as SegmentMap
import qualified Fox.Types.SegmentId as SegmentId

import qualified Control.Concurrent.MVar as MVar

data State
  = State {
        ixGeneration  :: !Generation.Generation
        -- ^ the current generation of the index.
      , ixSchema      :: !Schema.Schema
        -- ^ A mapping from fields to their types.
      , ixSegmentRefs :: !(SegmentMap.SegmentMap Int)
        -- ^ Hold reference counts for @Segment@s which
        -- are used in transactions.
      , ixSegments    :: !(SegmentMap.SegmentMap Segment.Segment)
        -- ^ @Segment@s contained in this @Index@.
      , ixSegIdGen     :: !SegmentId.SegIdGen
        -- ^ Generate new @SegmentId@s.
      }

-- | A synchronized, mutable reference to a @State@
type IxStateRef = MVar.MVar State
