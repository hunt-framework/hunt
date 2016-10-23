{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.SegmentIndex.Types where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet               (DocIdSet)
import           Hunt.Index.Schema
import           Hunt.SegmentIndex.Types.Index      (IndexRepr)
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap (SegmentMap)

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Data.Map                           (Map)
import           Prelude                            hiding (Word)

type ContextMap = Map Context IndexRepr

data Segment =
  Segment { segNumDocs     :: !Int
            -- ^ The number of documents in this 'Segment'
          , segNumTerms    :: !Int
            -- ^ The number of terms indexed by this 'Segment'.
            -- TODO: is this really needed? (Maybe a nice merge-metric)
          , segDeletedDocs :: !DocIdSet
            -- ^ The set of deleted 'DocId's
          , segDelGen      :: !Int
            -- ^ Since 'Segment's are immutable itself we have
            -- to remember at which delete generation we are.
          , segSchema      :: !Schema
            -- ^ Since the 'Schema' can be changed we also store it
            -- per 'Segment'.
          , segTermIndex   :: !ContextMap
            -- ^ Indexes the terms and points to the stored occurrences
          }

-- | 'IndexWriter' offers an interface to manipulate the index.
-- Forking new 'IndexWriter's from the 'SegmentIndex' *is* cheap.
data IndexWriter =
  IndexWriter { iwIndexDir    :: FilePath
                -- ^ When creating new 'Segment's we need to
                -- know where to place them.
              , iwNewSegId    :: IO SegmentId
                -- ^ The 'IndexWriter' commits 'Segment's to
                -- disk. So we need a way to generate unique
                -- 'SegmentId's.
              , iwSchema      :: Schema
                -- ^ The 'SegmentIndex' has a 'Schema'.
              , iwSegments    :: SegmentMap Segment
                -- ^ An 'IndexWriter' acts transactional over
                -- the 'SegmentIndex'. These are the 'Segment's
                -- present in the 'SegmentIndex' at the time
                -- this 'IndexWriter' was created. Basically
                -- this gives us the I from ACID.
              , iwNewSegments :: SegmentMap Segment
                -- ^ We have an upper limit on 'Document's buffered
                -- in the 'ContextIndex'. When the limit is hit
                -- we make the 'ContextIndex' a 'Segment' and
                -- flush it to disk.
                -- INVARIANT: 'iwSegments' and 'iwNewSegments' are
                -- disjoint in their 'SegmentId's.
                -- This 'IndexWriter' is the only one referencing
                -- this 'Segment's for now. This allows for easy
                -- merging and deleting 'Segment's if needed.
              , iwModSegments :: SegmentMap Segment
                -- ^ Everytime we delete documents we modify
                -- 'Segments' from 'iwSegments' and put it in 'iwModSegments'.
              , iwSegIxRef    :: SegIxRef
                -- ^ A reference to the 'SegmentIndex' which creates
                -- this 'IndexWriter'.
              }

-- | 'IndexReader' is query-only and doesn't modify the index in any way.
-- Its a saint compared to the 'IndexWriter'. Construction is *very* cheap.
data IndexReader =
  IndexReader { irSegments :: SegmentMap Segment
              }

-- | The 'SegmentIndex' holding everything together.
data SegmentIndex =
  SegmentIndex { siIndexDir :: !FilePath
                 -- ^ The directory where the 'Segment's and meta
                 -- data are stored.
               , siSegIdGen :: !SegIdGen
                 -- ^ 'IndexWriter's forked from the 'SegmentIndex'
                 -- need to create new 'Segment's (and hence 'SegmentId's).
                 -- This is a 'SegmentIndex' unique generator for 'SegmentId's.
               , siSchema   :: !Schema
                 -- ^ 'Schema' for indexed fields
               , siSegments :: !(SegmentMap Segment)
                 -- ^ The 'Segment's currently in the 'SegmentIndex'.
                 -- Since 'IndexWriter' and 'IndexReader' many reference
                 -- 'Segment's from the 'SegmentIndex' we *must not*
                 -- delete any 'Segment' of which we know its still
                 -- referenced. But we can safely merge any 'Segment'
                 -- in here as the merge result will not appear in
                 -- 'IndexReader' and 'IndexWriter'.
               , siSegRefs  :: !(SegmentMap Int)
                 -- ^ A map counting references to the 'Segment's.
                 -- We need to make sure we don't delete 'Segment's
                 --  from disk while someone might read from them.
                 -- INVARIANT: 'SegmentId's not present here are
                 -- assumed a count of 0
               }

-- | As forking of new 'IndexWriter' and 'IndexReader' involves no
-- IO we can store 'SegmentIndex' in a 'TVar'.
type SegIxRef = TVar SegmentIndex

-- | A mutex-locked reference to an 'IndexWriter'.
type IxWrRef = TMVar IndexWriter

-- | A type indicating the result of a transaction.
data CommitResult a = CommitOk a
                    | CommitConflicts [Conflict]

-- | The different conflict types which can arise.
data Conflict = ConflictDelete !SegmentId
