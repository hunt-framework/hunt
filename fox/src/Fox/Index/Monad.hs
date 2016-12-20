{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Fox.Index.Monad where

import           Fox.Analyze
import           Fox.Index.Directory
import           Fox.Types

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Map                   (Map)
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq


-- | Generate a new @DocId@ by incrementing.
type DocIdGen = DocId

-- | A synonym for an inverted index optimized for
-- insertions of terms.
type FieldIndex = Map Token (Map FieldName Occurrences)

-- | Every @Document@ indexed by an @IndexWrter@ goes into
-- the @Indexer@ first. It collects all kind of useful information
-- and inverts the index which can then be processed into a more
-- efficient form.
data Indexer = Indexer { indSchema   :: !Schema
                       , indDocIdGen :: !DocIdGen
                       , indIndex    :: !FieldIndex
                       , indDocs     :: !(Seq Document)
                       }

indNumDocs :: Indexer -> Int
indNumDocs ind = Seq.length (indDocs ind)

emptyIndexer :: Indexer
emptyIndexer = Indexer { indSchema   = mempty
                       , indDocIdGen = firstDocId
                       , indIndex    = mempty
                       , indDocs     = mempty
                       }

-- | A @Conflict@ occurs if two transactions changed the same
-- @Segment@s.
data Conflict = ConflictDelete SegmentId
              | ConflictFields FieldName FieldType FieldType

type Commit a = Either [Conflict] a

data Segment = Segment { segDelGen :: !Generation }

data ModifiedSegment =
  ModifiedSegment

-- | Executing a @PendingMerge@ waits until an already running
-- merge is finished. It is guaranteed to not throw an exception.
type PendingMerge = IO ()

-- | Configuration for @IndexWriter@.
data IxWrConfig =
  IxWrConfig { iwcMaxBufferedDocs :: !Int
               -- ^ The @IndexWriter@s @Indexer@ keeps @iwcMaxBufferedDocs@
               -- documents in memory before flushing to disk.
             }

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
          , iwAnalyzer :: !Analyzer
          -- ^ @Document@s are analyzed by this @Analyzer@.
          , iwConfig   :: !IxWrConfig
          }

data IxWrState =
  IxWrState { iwNewSegments :: !(SegmentMap Segment)
              -- ^ new @Segment@s created in an @IndexWriter@
              -- action. INVARIANT: `iwSegments` and `iwNewSegments`
              -- are disjoint.
            , iwModSegments :: !(SegmentMap Segment)
              -- ^ Everytime we delete documents from a @Segment@
              -- or update the fields weights we need to keep track
              -- of it here.
            , iwIndexer     :: !Indexer
            }

data ErrWriter = ErrConflict [Conflict]

-- | a write transaction over the @Index@. The @Index@ is updated
-- transactionally.
newtype IndexWriter a =
  IndexWriter { unIndexWriter ::
    forall r. (a -> IxWrState -> IO r) -> (ErrWriter -> IO r) -> IxWrEnv -> IxWrState -> IO r
  }

instance Functor IndexWriter where
  fmap f (IndexWriter m) = IndexWriter $ \succ_ fail_ env st ->
    m (\a st' -> succ_ (f a) st') fail_ env st
  {-# INLINE fmap #-}

instance Applicative IndexWriter where
  pure a = IndexWriter $ \succ_ _fail_ _env st -> succ_ a st
  {-# INLINE pure #-}

  IndexWriter mf <*> IndexWriter ma = IndexWriter $ \succ_ fail_ env st ->
    mf (\f st' -> ma (\a st'' -> succ_ (f a) st'') fail_ env st') fail_ env st
  {-# INLINE (<*>) #-}

instance Monad IndexWriter where
  IndexWriter m >>= f = IndexWriter $ \succ_ fail_ env st ->
    m (\a st' -> unIndexWriter (f a) succ_ fail_ env st) fail_ env st
  {-# INLINE (>>=) #-}

instance MonadIO IndexWriter where
  liftIO action = IndexWriter $ \succ_ fail_ env st -> do
    x <- action
    succ_ x st
  {-# INLINE liftIO #-}

errConflict :: Conflict -> IndexWriter a
errConflict conflict = errConflicts [conflict]

errConflicts :: [Conflict] -> IndexWriter a
errConflicts conflicts = IndexWriter $ \_succ_ fail_ _env _st ->
  fail_ (ErrConflict conflicts)

runIndexWriter :: IxWrEnv -> IxWrState -> IndexWriter a -> IO (Either ErrWriter (a, IxWrState))
runIndexWriter env st action =
  unIndexWriter action (\a _st -> pure $ Right (a, st)) (\e -> pure (Left e)) env st

askAnalyzer :: IndexWriter Analyzer
askAnalyzer = IndexWriter $ \succ_ _ env st -> succ_ (iwAnalyzer env) st

withAnalyzer :: Analyzer -> IndexWriter a -> IndexWriter a
withAnalyzer analyzer (IndexWriter m) = IndexWriter $ \succ_ fail_ env st ->
  m succ_ fail_ (env { iwAnalyzer = analyzer }) st

withIndexer :: (Indexer -> Either Conflict Indexer) -> IndexWriter ()
withIndexer f = IndexWriter $ \succ_ fail_ _env st ->
  case f (iwIndexer st) of
    Right indexer' -> succ_ () (st { iwIndexer = indexer' })
    Left conflict  -> fail_ $ ErrConflict [conflict]

askIndexer :: (Indexer -> a) -> IndexWriter a
askIndexer f = IndexWriter $ \succ_ _fail _env st ->
  succ_ (f (iwIndexer st)) st

askConfig :: (IxWrConfig -> a) -> IndexWriter a
askConfig f = IndexWriter $ \ succ_ _fail env st ->
  succ_ (f (iwConfig env)) st

askSchema :: IndexWriter Schema
askSchema = IndexWriter $ \succ_ _fail_ env st -> succ_ (iwSchema env) st


-- | A read-only transaction over the @Index@.
newtype IndexReader a =
  IndexReader { unIndexReader :: ReaderT (SegmentMap Segment) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
