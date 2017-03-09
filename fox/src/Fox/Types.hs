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

--  , Schema
--  , InternedSchema
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
import           Fox.Types.Term        (Term)
import qualified Fox.Types.Term        as Term

import           Data.HashMap.Strict   (HashMap)

-- | Indexed fields can be identified by number.
type FieldOrd = Int

-- | A @Schema@ keeps track which fields and their respective
-- type are in the index.
type Schema = HashMap FieldName FieldType

-- | A schema which helps interning field names to reduce duplicate
-- strings in memory.
type InternedSchema = HashMap FieldName (FieldName, FieldType)

-- | A @Token@ is a @Term@ annotated with a @Position@.
data Token = Token !Position !Term

nullToken :: Token -> Bool
nullToken (Token _ t) = Term.null t
