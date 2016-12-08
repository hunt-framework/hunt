module Fox.Types (
    Document(..)
  , DocDesc
  , mkDocDesc

  , SegmentId
  , firstSegmentId

  , Generation
  , nextGeneration

  , SegIdGen
  , newSegIdGen
  , genSegId

  , SegmentMap
  ) where

import           Fox.Types.DocDesc    (DocDesc, mkDocDesc)
import           Fox.Types.Document   (Document (..))
import           Fox.Types.Generation (Generation, firstGeneration,
                                       nextGeneration)
import           Fox.Types.SegmentId  (SegIdGen, SegmentId, firstSegmentId,
                                       genSegId, newSegIdGen)
import           Fox.Types.SegmentMap (SegmentMap)
