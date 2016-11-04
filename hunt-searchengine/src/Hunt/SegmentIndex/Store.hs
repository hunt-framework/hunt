{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.Store where

import           Hunt.Common.BasicTypes
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap

import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map

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
   } deriving (Show)

data SegmentInfos =
  SegmentInfos {
      sisSegmentIdGen :: !SegmentId
      -- ^ The next 'SegmentId' to generate
    , sisSegmentInfos :: !(SegmentMap SegmentInfo)
      -- ^ Meta data for construction of 'Segment's
    } deriving (Show)

segmentToSegmentInfo :: Segment -> SegmentInfo
segmentToSegmentInfo Segment{..} = SegmentInfo {
    segiNumDocs     = segNumDocs
  , segiDelGen      = segDelGen
  , segiContextInfo = Map.map indexReprNumTerms segTermIndex
  }
