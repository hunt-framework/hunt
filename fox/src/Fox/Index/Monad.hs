{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fox.Index.Monad where

import           Fox.Analyze
import           Fox.Index.Directory
import           Fox.Types

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict

data Segment = Segment { segDelGen :: !Generation }

data ModifiedSegment =
  ModifiedSegment

-- | Executing a @PendingMerge@ waits until an already running
-- merge is finished. It is guaranteed to not throw an exception.
type PendingMerge = IO ()

data IxWrEnv =
  IxWrEnv { iwIndexDir :: !IndexDirectory
          -- ^ When creating new @Segment@s we
          -- need to know where to place them.
          , iwNewSegId :: IO SegmentId
          -- ^ When creating new @Segment@s we
          -- need to be able to generate globally unique
          -- names for them.
          , iwSegments :: !(SegmentMap Segment)
          -- ^ An @IndexWriter@ action is transactional
          -- over the current state of the @Index@.
          , iwSchema   :: !Schema
          -- ^ We need to remember which fields have which types.
          }

data IxWrState =
  IxWrState { iwNewSchema :: !Schema
              -- ^ Adding new, unseen fields to the schema here.
              -- INVARIANT: doesn't contain any fields from iwSchema.
            , iwNewSegments   :: !(SegmentMap Segment)
              -- ^ new @Segment@s created in an @IndexWriter@
              -- action. INVARIANT: `iwSegments` and `iwNewSegments`
              -- are disjoint.
            , iwModSegments   :: !(SegmentMap Segment)
            -- ^ Everytime we delete documents from a @Segment@
            -- or update the fields weights we need to keep track
            -- of it here.
            , iwAnalyzer :: !Analyzer
            -- ^ @Document@s are analyzed by this @Analyzer@.
            }

-- | a write transaction over the @Index@. The @Index@ is updated
-- transactionally.
newtype IndexWriter a =
  IndexWriter { unIndexWriter :: ReaderT IxWrEnv (StateT IxWrState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runIndexWriter :: IxWrEnv -> IxWrState -> IndexWriter a -> IO (a, IxWrState)
runIndexWriter env st action =
  runStateT (runReaderT (unIndexWriter action) env) st

askNewSegments :: IndexWriter (SegmentMap Segment)
askNewSegments = IndexWriter $ gets iwNewSegments

askModSegments :: IndexWriter (SegmentMap Segment)
askModSegments = IndexWriter $ gets iwModSegments

newSegmentId :: IndexWriter SegmentId
newSegmentId = IndexWriter $ asks iwNewSegId >>= liftIO

-- | A read-only transaction over the @Index@.
newtype IndexReader a =
  IndexReader { unIndexReader :: ReaderT (SegmentMap Segment) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
