{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Fox.Index.Monad where

import           Fox.Analyze
import           Fox.Index.Directory
import           Fox.Schema             (Schema)
import           Fox.Types
import qualified Fox.Index.Segment      as Segment
import qualified Fox.Types.SegmentMap   as SegmentMap
import qualified Fox.Index.InvertedFile as InvertedFile

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           System.IO.Error (tryIOError)
import qualified Debug.Trace as Trace
import qualified GHC.Stack as CallStack

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
  IxWrConfig { iwcMaxBufferedDocs = 10
             }

data IxWrEnv =
  IxWrEnv { iwIndexDir :: !IndexDirectory
          -- ^ When creating new @Segment@s we
          -- need to know where to place them.
          , iwNewSegId :: IO SegmentId
          -- ^ When creating new @Segment@s we
          -- need to be able to generate globally unique
          -- names for them.
          , iwSegments :: !(SegmentMap Segment.Segment)
          -- ^ An @IndexWriter@ action is transactional
          -- over the current state of the @Index@.
          , iwAnalyzer :: !Analyzer
          -- ^ @Document@s are analyzed by this @Analyzer@.
          , iwConfig   :: !IxWrConfig
          }

data IxWrState =
  IxWrState { iwNewSegments :: !(SegmentMap Segment.Segment)
              -- ^ new or modified @Segment@s created in
              -- an @IndexWriter@ action.
            , iwDeletedDocs :: !(SegmentMap DocIdSet)
              -- ^ Remember deleted documents for each @Segment@.
            , iwSchema      :: !Schema
            -- ^ We need to remember which fields have which types
            }

data WriterError = WriterConflict [Conflict]
                 | WriterIOError IOError
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

trace :: (CallStack.HasCallStack, Show a)
      => a
      -> IndexWriter ()
trace x =
  CallStack.withFrozenCallStack $
    liftIO (Trace.traceM msg)
  where
    msg =
      concat [ show x
             , "\n"
             , CallStack.prettyCallStack CallStack.callStack
             ]

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

askEnv :: (IxWrEnv -> a) -> IndexWriter a
askEnv f = IndexWriter $ \succ_ _ env st -> succ_ (f env) st

newSegmentId :: IndexWriter SegmentId
newSegmentId = askEnv iwNewSegId >>= liftIO

askConfig :: (IxWrConfig -> a) -> IndexWriter a
askConfig f = IndexWriter $ \ succ_ _fail env st ->
  succ_ (f (iwConfig env)) st

askSchema :: IndexWriter Schema
askSchema = IndexWriter $ \succ_ _fail_ _ st ->
  succ_ (iwSchema st) st

setSchema :: Schema -> IndexWriter ()
setSchema schema = IndexWriter $ \succ_ _ _ st ->
  succ_ () (st { iwSchema = schema })

insertSegment :: SegmentId -> Segment.Segment -> IndexWriter ()
insertSegment segmentId segment = IndexWriter $ \succ_ _fail _ st ->
  succ_ () (st { iwNewSegments =
                  SegmentMap.insert segmentId segment  (iwNewSegments st)
              })

runIfM :: InvertedFile.IfM a -> IndexWriter a
runIfM action = IndexWriter $ \succ_ fail_ _ st -> do
  mresult <- tryIOError (InvertedFile.runIfM action)
  case mresult of
    Right a -> succ_ a st
    Left e  -> fail_ (WriterIOError e)

-- | A read-only transaction over the @Index@.
newtype IndexReader a =
  IndexReader { unIndexReader :: ReaderT (SegmentMap Segment.Segment) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
