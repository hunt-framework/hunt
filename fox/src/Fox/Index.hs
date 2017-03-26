{-# LANGUAGE RecordWildCards #-}
module Fox.Index (
    IndexRef
  , newIndex
  , runWriter

  , defaultAnalyzer
  ) where

import           Fox.Analyze
import           Fox.Index.Directory
import           Fox.Index.Monad
import           Fox.Indexer             (emptyIndexer, indSchema)
import           Fox.Schema              (Schema, checkSchemaTys, emptySchema)
import           Fox.Types
import qualified Fox.Types.SegmentMap    as SegmentMap

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Except
import qualified Data.List               as List

-- | A synchronized, mutable reference to an @Index@
newtype IndexRef = IndexRef (MVar Index)

-- | @Index@ holds the meta data for the actual index.
data Index =
  Index { ixIndexDir     :: !IndexDirectory
          -- ^ the directory where the indexed data resides.
        , ixSegments     :: !(SegmentMap Segment)
          -- ^ @Segment@s contained in this @Index@.
        , ixSegmentRefs  :: !(SegmentMap Int)
          -- ^ Hold reference counts for @Segment@s which
          -- are used in transactions.
        , ixSegIdGen     :: !SegIdGen
          -- ^ Generate new @SegmentId@s.
        , ixSchema       :: !Schema
          -- ^ A mapping from fields to their types.
        , ixWriterConfig :: !IxWrConfig
        }

-- | Default @Analyzer@ tokenizes non-empty alpha-numeric
-- terms.
defaultAnalyzer :: Analyzer
defaultAnalyzer = newAnalyzer tokenizeAlpha filterNonEmpty

-- | Create a new index in a specified @IndexDirectory@.
-- Does not load index if one exists.
newIndex :: IndexDirectory -> IO IndexRef
newIndex indexDirectory = do
  segIdGen <- newSegIdGen
  index    <- newMVar $! Index { ixIndexDir     = indexDirectory
                               , ixSegments     = SegmentMap.empty
                               , ixSegmentRefs  = SegmentMap.empty
                               , ixSegIdGen     = segIdGen
                               , ixSchema       = emptySchema
                               , ixWriterConfig = defaultWriterConfig
                               }
  return $! (IndexRef index)

-- | Run an @IndexWriter@ transaction over the @Index@.
-- This only locks the @Index@ only in conflict checking
-- phase so multiple concurrent @runIndexWriter@ calls
-- are possible.
runWriter :: Analyzer -> IndexRef -> IndexWriter a -> IO (Commit a)
runWriter analyzer indexRef indexWriter = do
  -- we better bracket here. If there is an exception in
  -- the transaction we better dereference the referenced
  -- segments.
  bracket (forkIxWrEnv analyzer) closeIxWrEnv $ \writerEnv -> do
    let
      writerState =
        IxWrState { iwNewSegments   = SegmentMap.empty
                  , iwDeletedDocs   = SegmentMap.empty
                  , iwIndexer       = emptyIndexer
                  }

    -- run the transaction. Note that it can start merges
    -- by itself.
    mresult <- runIndexWriter writerEnv writerState indexWriter
    case mresult of
      Right (result, writerState') -> do
        modIndex indexRef $ \index ->
          case commit index writerEnv writerState' of
            Right index'   -> (index', return result)
            Left conflicts -> (index, throwError conflicts)
      Left err -> throwIO err
      where
    -- Fork an environment for a new transaction
    forkIxWrEnv :: Analyzer -> IO IxWrEnv
    forkIxWrEnv anal = modIndex indexRef $ \index ->
      let
        -- increase the ref count for every Segment.
        -- so we don't garbage collect any Segments
        -- used in a transaction.
        index' = index { ixSegmentRefs =
                           SegmentMap.unionWith
                           (+)
                           (SegmentMap.map (\_ -> 1) (ixSegments index))
                           (ixSegmentRefs index)
                       }

        env = IxWrEnv { iwIndexDir = ixIndexDir index
                      , iwNewSegId = genSegId (ixSegIdGen index)
                      , iwSegments = ixSegments index
                      , iwSchema   = ixSchema index -- TODO: do we really need this?
                      , iwAnalyzer = anal
                      , iwConfig   = ixWriterConfig index
                      }
      in (index', env)

    closeIxWrEnv :: IxWrEnv -> IO ()
    closeIxWrEnv env = modIndex indexRef $ \index ->
      let
        -- delete segments from ixSegmentRefs if they
        -- are not referenced anywhere.
        deleteIfZero n
          | n - 1 > 0 = Just (n - 1)
          | otherwise = Nothing

        index' = index { ixSegmentRefs =
                           SegmentMap.differenceWith
                           (\n _ -> deleteIfZero n)
                           (ixSegmentRefs index)
                           (iwSegments env)
                       }
      in (index', ())

    commit :: Index -> IxWrEnv -> IxWrState -> Commit Index
    commit index@Index{..} IxWrEnv{..} IxWrState{..} =
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
              | segDelGen old /= segDelGen new
              ]

        -- check whether we have conflicting field definitions.
        schemaConflicts :: [Conflict]
        schemaConflicts =
          [ ConflictFields fieldName fieldTy fieldTy'
          | (fieldName, fieldTy, fieldTy') <-
              checkSchemaTys (indSchema iwIndexer) ixSchema
          ]

        mergedSegments :: SegmentMap Segment
        mergedSegments =
          SegmentMap.unionWith (\new _old -> new) iwNewSegments ixSegments

      in case schemaConflicts ++ indexConflicts of
        [] -> return $! index { ixSegments = mergedSegments }
        xs -> throwError xs

modIndex :: IndexRef -> (Index -> (Index, a)) -> IO a
modIndex (IndexRef indexRef) f =
  modifyMVar indexRef $ \index -> return $! f index
