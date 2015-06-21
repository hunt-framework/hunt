{-# LANGUAGE OverloadedStrings #-}
module Hunt.ContextIndex.Status where

import           Data.Aeson
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocIdMap as DocIdMap
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Segment
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable

data Status
  = Status { csNumberOfSegments   :: !Int
           , csSegmentStatus      :: ![SegmentStatus]
           }

data SegmentStatus
  = SegmentStatus { ssId               :: !SegmentId
                  , ssDeletedContexts  :: !(Set Context)
                  , ssDeletedDocs      :: !DocIdSet
                  , ssSegmentSize      :: !Int
                  , ssDeletedDocsRatio :: !Float
                  }

status :: (Monad m, DocTable dt) => ContextIndex dt -> m Status
status ixx
  = do ssx <- mapM (uncurry segmentStatus) (toList (ciSegments ixx))
       return Status { csNumberOfSegments   = size (ciSegments ixx)
                     , csSegmentStatus      = ssx
                     }

segmentStatus :: (Monad m, DocTable dt) => SegmentId -> Segment dt -> m SegmentStatus
segmentStatus sid seg
  = do size  <- segmentSize seg
       ratio <- segmentDeletedDocsRatio seg
       return SegmentStatus { ssId               = sid
                            , ssDeletedContexts  = segDeletedCxs seg
                            , ssDeletedDocs      = segDeletedDocs seg
                            , ssSegmentSize      = size
                            , ssDeletedDocsRatio = ratio
                            }

instance ToJSON SegmentStatus where
  toJSON ss
    = object [ "id"               .= ssId ss
             , "deletedDocs"      .= ssDeletedDocs ss
             , "deletedContexts"  .= ssDeletedContexts ss
             , "size"             .= ssSegmentSize ss
             , "deletedDocsRatio" .= ssDeletedDocsRatio ss
             ]

instance ToJSON Status where
  toJSON s
    = object [ "segmentCount"       .= csNumberOfSegments s
             , "segments"           .= csSegmentStatus s
             ]

instance ToJSON SegmentId where
  toJSON (SegmentId x) = toJSON x
