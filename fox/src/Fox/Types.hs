module Fox.Types (
    Document(..)
  , DocField(..)
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

  , Schema

  ) where

import           Fox.Types.DocDesc    (DocDesc, mkDocDesc)
import           Fox.Types.DocDesc    (FieldName, FieldType (..),
                                       FieldValue (..))
import           Fox.Types.Document   (DocField (..), Document (..))
import           Fox.Types.Generation (Generation, firstGeneration,
                                       nextGeneration)
import           Fox.Types.SegmentId  (SegIdGen, SegmentId, firstSegmentId,
                                       genSegId, newSegIdGen)
import           Fox.Types.SegmentMap (SegmentMap, SegmentSet)

import           Data.HashMap.Strict  (HashMap)

-- | A @Schema@ keeps track which fields and their respective
-- type are in the index.
type Schema = HashMap FieldName FieldType
