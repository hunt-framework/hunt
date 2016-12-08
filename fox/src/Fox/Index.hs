module Fox.Index where

import           Fox.Types

import           Data.Primitive.PrimRef



type Index = MVar (MutIndex (GenShardedIndex Segment))



data MutIndex a = MutIndex {
    mixIndexDir   :: !FilePath
  , mixGeneration :: !Generation
  , mixIndex      :: a
  }

-- | The @ShardIndex@ holding everything together. It is parametric
-- in 'a' which is used to hold different representations for 'Segment's.
data GenShardedIndex a =
  ShardedIndex { siSegIdGen   :: !SegIdGen
                 -- ^ 'IndexWriter's forked from the 'SegmentIndex'
                 -- need to create new 'Segment's (and hence 'SegmentId's).
                 -- This is a 'SegmentIndex' unique generator for 'SegmentId's.
               , siSchema     :: !Schema
                 -- ^ 'Schema' for indexed fields
               , siSegments   :: !(SegmentMap a)
                 -- ^ The 'Segment's currently in the 'SegmentIndex'.
                 -- Since 'IndexWriter' and 'IndexReader' many reference
                 -- 'Segment's from the 'SegmentIndex' we *must not*
                 -- delete any 'Segment' of which we know its still
                 -- referenced. But we can safely merge any 'Segment'
                 -- in here as the merge result will not appear in
                 -- 'IndexReader' and 'IndexWriter'.
               , siSegRefs    :: !(SegmentMap Int)
                 -- ^ A map counting references to the 'Segment's.
                 -- We need to make sure we don't delete 'Segment's
                 --  from disk while someone might read from them.
                 -- INVARIANT: 'SegmentId's not present here are
                 -- assumed a count of 0
               }
