module Fox.Types (
    Document(..)
  , DocField
  , FieldName
  , FieldType(..)
  , FieldValue(..)
  , DocDesc
  , mkDocDesc

  , SegmentId
  , firstSegmentId

  , Generation
  , firstGeneration
  , nextGeneration

  , SegIdGen
  , newSegIdGen
  , genSegId

  , SegmentMap
  , SegmentSet
  ) where

import           Fox.Types.DocDesc    (DocDesc, mkDocDesc)
import           Fox.Types.DocDesc    (FieldName, FieldValue (..))
import           Fox.Types.Document   (DocField (..), Document (..),
                                       FieldType (..))
import           Fox.Types.Generation (Generation, firstGeneration,
                                       nextGeneration)
import           Fox.Types.SegmentId  (SegIdGen, SegmentId, firstSegmentId,
                                       genSegId, newSegIdGen)
import           Fox.Types.SegmentMap (SegmentMap, SegmentSet)
