module Fox.Types (
    DocId
  , firstDocId
  , nextDocId

  , Document(..)
  , DocField(..)
  , FieldName
  , FieldOrd
  , FieldType(..)
  , FieldValue(..)
  , fieldType
  , dfType
  , SegmentId
  , firstSegmentId

  , Term
  , Token(..)
  , nullToken

  , DocIdSet
  , DocIdMap
  , Occurrences

  , Position
  , Positions

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

import           Fox.Types.DocDesc     (FieldName, FieldType (..),
                                        FieldValue (..), fieldType)
import           Fox.Types.DocIdMap    (DocIdMap)
import           Fox.Types.DocIdSet    (DocIdSet)
import           Fox.Types.Document    (DocField (..), DocId, Document (..),
                                        dfType, firstDocId, nextDocId)
import           Fox.Types.Generation  (Generation, firstGeneration,
                                        nextGeneration)
import           Fox.Types.Occurrences (Occurrences)
import           Fox.Types.Positions   (Position, Positions)
import           Fox.Types.SegmentId   (SegIdGen, SegmentId, firstSegmentId,
                                        genSegId, newSegIdGen)
import           Fox.Types.SegmentMap  (SegmentMap, SegmentSet)

import           Data.HashMap.Strict   (HashMap)
import           Data.Text             (Text)
import qualified Data.Text             as Text

-- | Indexed fields can be identified by number.
type FieldOrd = Int

-- | A @Schema@ keeps track which fields and their respective
-- type are in the index.
type Schema = HashMap FieldName FieldType

-- | A sequence of bytes suitable for indexing
type Term = Text

-- | A @Token@ is a @Term@ annotated with a @Position@.
data Token = Token !Position !Term
           deriving (Eq, Show)

nullToken :: Token -> Bool
nullToken (Token _ t) = Text.null t
