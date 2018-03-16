{-# LANGUAGE RecordWildCards #-}
module Fox.Index (
    IndexRef
  , openIndex
  , runWriter

  , Directory.IndexDirectory
  , Directory.defaultIndexDirectory

  , defaultAnalyzer
  ) where

import           Fox.Analyze
import           Fox.Index.Monad
import           Fox.Schema              (Schema, diffCommonFields, emptySchema)
import           Fox.Types
import qualified Fox.Index.Directory     as Directory
import qualified Fox.Index.MetaFile      as MetaFile
import qualified Fox.Index.Segment       as Segment
import qualified Fox.Index.State         as State
import qualified Fox.Schema as Schema
import qualified Fox.Types.Generation    as Generation
import qualified Fox.Types.SegmentMap    as SegmentMap

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Except
import qualified Data.List               as List

-- | A synchronized, mutable reference to an @Index@
newtype IndexRef = IndexRef Index

-- | @Index@ holds the meta data for the actual index.
data Index =
  Index { ixIndexDir     :: !Directory.IndexDirectory
          -- ^ the directory where the indexed data resides.
        , ixWriterConfig :: !IxWrConfig
          -- ^ Configuration for the @IndexWriter@s
        , ixState        :: !State.IxStateRef
        }

-- | Default @Analyzer@ tokenizes non-empty alpha-numeric
-- terms.
defaultAnalyzer :: Analyzer
defaultAnalyzer = newAnalyzer tokenizeAlpha filterNonEmpty

-- | Create a new index in a specified @IndexDirectory@.
-- Does not load index if one exists.
openIndex :: Directory.IndexDirectory -> IO IndexRef
openIndex indexDirectory = do

  ixState <- openIndexState indexDirectory

  let
    index =
      Index {
          ixIndexDir     = indexDirectory
        , ixWriterConfig = defaultWriterConfig
        , ixState        = ixState
        }

  return $! (IndexRef index)

openIndexState :: Directory.IndexDirectory -> IO State.IxStateRef
openIndexState indexDirectory = do

  Directory.createIndexDirectory indexDirectory

  metaFiles <- Directory.listMetaFiles indexDirectory

  ixState <- case metaFiles of
    ((_, mostRecentMetaFile):_) -> do
      MetaFile.runMfM $ do
        mstate <- MetaFile.readIndexMetaFile mostRecentMetaFile
        case mstate of
          Right state -> return state
          Left err    -> throwIO err
    [] -> do
      segIdGen <- newSegIdGen

      let
        indexState =
          State.State {
            State.ixGeneration  = Generation.genesis
          , State.ixSchema      = emptySchema
          , State.ixSegmentRefs = SegmentMap.empty
          , State.ixSegments    = SegmentMap.empty
          , State.ixSegIdGen    = segIdGen
          }

      return indexState

  state <- newMVar $! ixState
  return state

-- | Run an @IndexWriter@ transaction over the @Index@.
-- This only locks the @Index@ only in conflict checking
-- phase so multiple concurrent @runIndexWriter@ calls
-- are possible.
runWriter :: Analyzer -> IndexRef -> IndexWriter a -> IO (Commit a)
runWriter analyzer (IndexRef indexRef) indexWriter = do
  -- we better bracket here. If there is an exception in
  -- the transaction we better dereference the referenced
  -- segments.
  bracket (forkIxWrEnv analyzer) closeIxWrEnv $ \(schema, writerEnv) -> do
    let
      writerState =
        IxWrState { iwNewSegments   = SegmentMap.empty
                  , iwDeletedDocs   = SegmentMap.empty
                  , iwSchema        = schema
                  }

    -- run the transaction. Note that it can start merges
    -- by itself.
    mresult <- runIndexWriter writerEnv writerState indexWriter
    case mresult of
      Right (result, writerState') -> do
        mcommit <- modIndex indexRef $ \index ->
          case commit index writerEnv writerState' of
            Right index'   -> (index', return (index', result))
            Left conflicts -> (index, throwError conflicts)

        case mcommit of
          Right (ixState', r) -> do
            let
              metaDirLayout =
                Directory.metaDirLayout (ixIndexDir indexRef)

            MetaFile.runMfM $ do
              MetaFile.writeIndexMetaFile metaDirLayout ixState'
            return (Right r)
          Left conflicts ->
            return (Left conflicts)
      Left err -> throwIO err
      where
    -- Fork an environment for a new transaction
    forkIxWrEnv :: Analyzer -> IO (Schema, IxWrEnv)
    forkIxWrEnv anal = modIndex indexRef $ \index ->
      let
        -- increase the ref count for every Segment.
        -- so we don't garbage collect any Segments
        -- used in a transaction.
        index' = index { State.ixSegmentRefs =
                           SegmentMap.unionWith
                           (+)
                           (SegmentMap.map (\_ -> 1) (State.ixSegments index))
                           (State.ixSegmentRefs index)
                       }

        env = IxWrEnv { iwIndexDir = ixIndexDir indexRef
                      , iwNewSegId = genSegId (State.ixSegIdGen index)
                      , iwSegments = State.ixSegments index
                      , iwAnalyzer = anal
                      , iwConfig   = ixWriterConfig indexRef
                      }
      in (index', (State.ixSchema index, env))

    closeIxWrEnv :: (Schema, IxWrEnv) -> IO ()
    closeIxWrEnv (_, env) = modIndex indexRef $ \index ->
      let
        -- delete segments from ixSegmentRefs if they
        -- are not referenced anywhere.
        deleteIfZero n
          | n - 1 > 0 = Just $! (n - 1)
          | otherwise = Nothing

        index' = index { State.ixSegmentRefs =
                           SegmentMap.differenceWith
                           (\n _ -> deleteIfZero n)
                           (State.ixSegmentRefs index)
                           (iwSegments env)
                       }
      in (index', ())

    -- add the new and modified segments to the index.
    -- Fails on any kind of conflict.
    commit :: State.State -> IxWrEnv -> IxWrState -> Commit State.State
    commit index@State.State{..} IxWrEnv{..} IxWrState{..} =
      let
        -- checks for conflcts on two segments
        indexConflicts :: [Conflict]
        indexConflicts =
          List.concat
          $ SegmentMap.elems
          $ SegmentMap.intersectionWithKey check ixSegments iwNewSegments
          where
            check segmentId old new =
              [ ConflictDelete segmentId
              | Segment.segGeneration old /= Segment.segGeneration new
              ]

        -- check whether we have conflicting field definitions.
        schemaConflicts :: [Conflict]
        schemaConflicts =
          [ ConflictFields fieldName fieldTy fieldTy'
          | (fieldName, fieldTy, fieldTy') <-
              diffCommonFields iwSchema ixSchema
          ]

        mergedSegments :: SegmentMap Segment.Segment
        mergedSegments =
          SegmentMap.unionWith (\new _old -> new) iwNewSegments ixSegments

      in case schemaConflicts ++ indexConflicts of
        [] -> return $! index { State.ixGeneration =
                                  Generation.nextGeneration (State.ixGeneration index)
                              , State.ixSegments =
                                  mergedSegments
                              , State.ixSchema   =
                                  Schema.union iwSchema ixSchema
                              }
        xs -> throwError xs

modIndex :: Index -> (State.State -> (State.State, a)) -> IO a
modIndex indexRef f =
  modifyMVar (ixState indexRef) $ \index -> return $! f index
