{-# LANGUAGE OverloadedStrings #-}
module Hunt.ContextIndex.Status where

import           Data.Aeson
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable

data Status
  = Status { csNumberOfSegments   :: !Int
           , csSegmentStatus      :: ![SegmentStatus]
           }

data SegmentStatus
  = SegmentStatus { ssId              :: !SegmentId
                  , ssState           :: !SegmentState
                  , ssDeletedContexts :: !(Set Context)
                  , ssDeletedDocs     :: !DocIdSet
                  }

status :: (Monad m, DocTable dt) => ContextIndex dt -> m Status
status ixx
  = do ssx <- mapM segmentStatus (ciSegments ixx)
       return Status { csNumberOfSegments   = List.length (ciSegments ixx)
                     , csSegmentStatus      = ssx
                     }

segmentStatus :: (Monad m, DocTable dt) => Segment dt -> m SegmentStatus
segmentStatus seg
  = return SegmentStatus { ssId              = segId seg
                         , ssState           = segState seg
                         , ssDeletedContexts = segDeletedCxs seg
                         , ssDeletedDocs     = segDeletedDocs seg
                         }

instance ToJSON SegmentStatus where
  toJSON ss
    = object [ "id"              .= ssId ss
             , "isDirty"         .= ssState ss
             , "deletedDocs"     .= ssDeletedDocs ss
             , "deletedContexts" .= ssDeletedContexts ss
             ]

instance ToJSON Status where
  toJSON s
    = object [ "segmentCount"       .= csNumberOfSegments s
             , "segments"           .= csSegmentStatus s
             ]

instance ToJSON SegmentId where
  toJSON (SegmentId x) = toJSON x

instance ToJSON SegmentState where
  toJSON = toJSON . show
