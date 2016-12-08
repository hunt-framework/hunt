{-# LANGUAGE RecordWildCards #-}
module Fox.Index where

import           Fox.Index.Directory
import           Fox.Index.Monad
import           Fox.Types
import qualified Fox.Types.SegmentMap    as SegmentMap

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Except
import qualified Data.List               as List

type IndexRef = MVar Index

data Index =
  Index { ixIndexDir    :: !IndexDirectory
        , ixSegments    :: !(SegmentMap Segment)
        , ixSegmentRefs :: !(SegmentMap Int)
        , ixSegIdGen    :: !SegIdGen
        }

data Conflict = ConflictDelete SegmentId

type Commit a = Either [Conflict] a

-- | Run an @IndexWriter@ transaction over the @Index@.
-- This only locks the @Index@ only in conflict checking
-- phase so multiple concurrent @runIndexWriter@ calls
-- are possible.
runWriter :: IndexRef -> IndexWriter a -> IO (Commit a)
runWriter indexRef indexWriter = do
  -- we better bracket here. If there is an exception in
  -- the transaction we better dereference the referenced
  -- segments.
  bracket forkIxWrEnv closeIxWrEnv $ \writerEnv -> do
    let writerState =
          IxWrState { iwNewSegments   = SegmentMap.empty
                    , iwModSegments   = SegmentMap.empty
                    }

    -- run the transaction. Note that it can start merges
    -- by itself.
    (result, writerState') <- runIndexWriter writerEnv writerState indexWriter

    modIndex indexRef $ \index ->
      case commit index writerEnv writerState' of
        Right index'   -> (index', return result)
        Left conflicts -> (index, throwError conflicts)
  where
    -- Fork an environment for a new transaction
    forkIxWrEnv :: IO IxWrEnv
    forkIxWrEnv = modIndex indexRef $ \index ->
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
        checkConflict :: SegmentId -> Segment -> Segment -> [Conflict]
        checkConflict sid old new =
          -- currently only the delete generations can be changed
          -- so if we have modifed 'Segment's here they always conflict.
          [ ConflictDelete sid
          | segDelGen old /= segDelGen new
          ]

        conflicts :: [Conflict]
        conflicts =
          List.concat
          $ SegmentMap.elems
          $ SegmentMap.intersectionWithKey checkConflict ixSegments
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
