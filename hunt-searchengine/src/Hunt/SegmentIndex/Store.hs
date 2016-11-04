{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.Store where

import           GHC.Generics

import           Hunt.Common.BasicTypes
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Store

-- | A simplified representation for 'Segment' which is used
-- to store 'Segment's on disk.
data SegmentInfo =
  SegmentInfo {
     segiNumDocs     :: !Int
     -- ^ The number of documents in this 'Segment'
   , segiDelGen      :: !Generation
     -- ^ The delete 'Generation'
   , segiContextInfo :: !(Map Context Int)
     -- ^ The number of terms stored per context
   } deriving (Show, Generic)

instance Store SegmentInfo

data SegmentInfos =
  SegmentInfos {
      sisSegmentIdGen :: !SegmentId
      -- ^ The next 'SegmentId' to generate
    , sisSegmentInfos :: !(SegmentMap SegmentInfo)
      -- ^ Meta data for construction of 'Segment's
    } deriving (Show, Generic)

instance Store SegmentInfos

segmentToSegmentInfo :: Segment -> SegmentInfo
segmentToSegmentInfo Segment{..} = SegmentInfo {
    segiNumDocs     = segNumDocs
  , segiDelGen      = segDelGen
  , segiContextInfo = Map.map indexReprNumTerms segTermIndex
  }

segmentIndexToSegmentInfos :: SegmentIndex -> IO SegmentInfos
segmentIndexToSegmentInfos SegmentIndex{..} = do
  nextSegmentId <- genSegId siSegIdGen
  return SegmentInfos {
      sisSegmentIdGen = nextSegmentId
    , sisSegmentInfos = SegmentMap.map segmentToSegmentInfo siSegments
    }
