{-# LANGUAGE RecordWildCards #-}
module Fox.Index where

import           Fox.Analyze
import           Fox.Index.Directory
import           Fox.Index.Monad
import           Fox.Types
import qualified Fox.Types.SegmentMap    as SegmentMap

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Except
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.List               as List

-- | A synchronized, mutable reference to an @Index@
type IndexRef = MVar Index

-- | @Index@ holds the meta data for the actual index.
data Index =
  Index { ixIndexDir    :: !IndexDirectory
          -- ^ the directory where the indexed data resides.
        , ixSegments    :: !(SegmentMap Segment)
          -- ^ @Segment@s contained in this @Index@.
        , ixSegmentRefs :: !(SegmentMap Int)
          -- ^ Hold reference counts for @Segment@s which
          -- are used in transactions.
        , ixSegIdGen    :: !SegIdGen
          -- ^ Generate new @SegmentId@s.
        , ixSchema      :: !Schema
          -- ^ A mapping from fields to their types.
        }


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
                  , iwModSegments   = SegmentMap.empty
                  , iwIndexer       = emptyIndexer
                  }

    -- run the transaction. Note that it can start merges
    -- by itself.
    Right (result, writerState') <- runIndexWriter writerEnv writerState indexWriter

    modIndex indexRef $ \index ->
      case commit index writerEnv writerState' of
        Right index'   -> (index', return result)
        Left conflicts -> (index, throwError conflicts)
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
                      , iwSchema   = ixSchema index
                      , iwAnalyzer = anal
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
        checkDelConflict :: SegmentId -> Segment -> Segment -> [Conflict]
        checkDelConflict sid old new =
          -- currently only the delete generations can be changed
          -- so if we have modifed 'Segment's here they always conflict.
          [ ConflictDelete sid
          | segDelGen old /= segDelGen new
          ]

        -- check whether we have conflicting field definitions.
        checkSchemaConflict :: [Conflict]
        checkSchemaConflict =
          mconcat
          $ HashMap.elems
          $ HashMap.intersectionWithKey check (indSchema iwIndexer) ixSchema
          where
            check fieldName fieldTy1 fieldTy2
              | fieldTy1 /= fieldTy2 = [ConflictFields fieldName fieldTy1 fieldTy2]
              | otherwise            = []

        conflicts :: [Conflict]
        conflicts =
          List.concat
          $ SegmentMap.elems
          $ SegmentMap.intersectionWithKey checkDelConflict ixSegments
          $ SegmentMap.intersection iwSegments iwModSegments

        mergedSegments :: SegmentMap Segment
        mergedSegments =
          SegmentMap.unionWith (\new _old -> new) iwModSegments
          $ SegmentMap.union iwModSegments ixSegments

      in case conflicts of
        [] -> return $! index { ixSegments = mergedSegments }
        _  -> throwError conflicts

modIndex :: IndexRef -> (Index -> (Index, a)) -> IO a
modIndex indexRef f =
  modifyMVar indexRef $ \index -> return $! f index
