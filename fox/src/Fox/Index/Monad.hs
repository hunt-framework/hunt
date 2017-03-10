{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Fox.Index.Monad where

import           Fox.Analyze
import           Fox.Index.Directory
import           Fox.Schema             (Schema, emptySchema)
import           Fox.Types
import qualified Fox.Types.SegmentMap   as SegmentMap

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq

-- | Generate a new @DocId@ by incrementing.
type DocIdGen = DocId

-- | A synonym for an inverted index optimized for
-- insertions of terms.
type FieldIndex = Map Term (Map FieldName Occurrences)

type BufferedDocs = Seq Document

-- | Every @Document@ indexed by an @IndexWrter@ goes into
-- the @Indexer@ first. It collects all kind of useful information
-- and inverts the index which can then be processed into a more
-- efficient form.
data Indexer = Indexer { indDocIdGen :: !DocIdGen
                       , indIndex    :: !FieldIndex
                       , indDocs     :: !BufferedDocs
                       , indSchema   :: !Schema
                       } deriving (Show)

indNumDocs :: Indexer -> Int
indNumDocs ind = Seq.length (indDocs ind)

-- | Check if an @Indexer@ has no buffered documents.
indNull :: Indexer -> Bool
indNull ind = Map.null (indIndex ind)

emptyIndexer :: Indexer
emptyIndexer =
  Indexer { indDocIdGen = firstDocId
          , indIndex    = mempty
          , indDocs     = mempty
          , indSchema   = emptySchema
          }

indexerSchema :: Indexer -> Schema
indexerSchema indexer = indSchema indexer

-- | A @Conflict@ occurs if two transactions changed the same
-- @Segment@s.
data Conflict = ConflictDelete SegmentId
              | ConflictFields FieldName FieldType FieldType
              deriving (Show)

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

defaultWriterConfig :: IxWrConfig
defaultWriterConfig =
  IxWrConfig { iwcMaxBufferedDocs = 0
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
              -- ^ new or modified @Segment@s created in
              -- an @IndexWriter@ action.
            , iwDeletedDocs :: !(SegmentMap DocIdSet)
              -- ^ Remember deleted documents for each @Segment@.
            , iwIndexer     :: !Indexer
            }

data WriterError = WriterConflict [Conflict]
                 | WriterIDirErr IDirError
                 deriving (Show)

instance Exception WriterError


-- | a write transaction over the @Index@. The @Index@ is updated
-- transactionally.
newtype IndexWriter a =
  IndexWriter { unIndexWriter :: forall r. (a -> IxWrState -> IO r)
                              -> (WriterError -> IO r)
                              -> IxWrEnv
                              -> IxWrState
                              -> IO r
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
    m (\a st' -> unIndexWriter (f a) succ_ fail_ env st') fail_ env st
  {-# INLINE (>>=) #-}

instance MonadIO IndexWriter where
  liftIO action = IndexWriter $ \succ_ _ _ st -> do
    x <- action
    succ_ x st
  {-# INLINE liftIO #-}

errConflict :: Conflict -> IndexWriter a
errConflict conflict = errConflicts [conflict]

errConflicts :: [Conflict] -> IndexWriter a
errConflicts conflicts = IndexWriter $ \_succ_ fail_ _env _st ->
  fail_ (WriterConflict conflicts)

runIndexWriter :: IxWrEnv
               -> IxWrState
               -> IndexWriter a
               -> IO (Either WriterError (a, IxWrState))
runIndexWriter env st action =
  unIndexWriter action (\a st' -> pure $ Right (a, st')) (\e -> pure (Left e)) env st

askAnalyzer :: IndexWriter Analyzer
askAnalyzer = IndexWriter $ \succ_ _ env st -> succ_ (iwAnalyzer env) st

withAnalyzer :: Analyzer -> IndexWriter a -> IndexWriter a
withAnalyzer analyzer (IndexWriter m) = IndexWriter $ \succ_ fail_ env st ->
  m succ_ fail_ (env { iwAnalyzer = analyzer }) st

withIndexer :: (Indexer -> Either Conflict Indexer) -> IndexWriter ()
withIndexer f = IndexWriter $ \succ_ fail_ _env st ->
  case f (iwIndexer st) of
    Right indexer' -> succ_ () (st { iwIndexer = indexer' })
    Left conflict  -> fail_ $ WriterConflict [conflict]

withIndexDirectory :: IDir a -> IndexWriter a
withIndexDirectory f = askEnv iwIndexDir >>= \indexDirectory -> do
  IndexWriter $ \succ_ fail_ _ st -> do
    result <- runIDir indexDirectory f
    case result of
      Right a -> succ_ a st
      Left e  -> fail_ (WriterIDirErr e)

askEnv :: (IxWrEnv -> a) -> IndexWriter a
askEnv f = IndexWriter $ \succ_ _ env st -> succ_ (f env) st

newSegmentId :: IndexWriter SegmentId
newSegmentId = askEnv iwNewSegId >>= liftIO

askIndexer :: (Indexer -> a) -> IndexWriter a
askIndexer f = IndexWriter $ \succ_ _fail _env st ->
  succ_ (f (iwIndexer st)) st

askConfig :: (IxWrConfig -> a) -> IndexWriter a
askConfig f = IndexWriter $ \ succ_ _fail env st ->
  succ_ (f (iwConfig env)) st

askSchema :: IndexWriter Schema
askSchema = IndexWriter $ \succ_ _fail_ env st ->
  succ_ (iwSchema env) st

insertSegment :: SegmentId -> Segment -> IndexWriter ()
insertSegment segmentId segment = IndexWriter $ \succ_ _fail _ st ->
  succ_ () (st { iwNewSegments =
                  SegmentMap.insert segmentId segment  (iwNewSegments st)
              })

-- | A read-only transaction over the @Index@.
newtype IndexReader a =
  IndexReader { unIndexReader :: ReaderT (SegmentMap Segment) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
