module Fox.Types (
    DocId
  , firstDocId
  , nextDocId

  , Document(..)
  , DocField(..)
  , FieldName
  , FieldType(..)
  , FieldValue(..)
  , fieldType
  , dfType
  , SegmentId
  , firstSegmentId

  , Term
  , Token(..)

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

  , Conflict(..)
  , Commit
  ) where

import           Fox.Types.Conflicts   (Commit, Conflict (..))
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

import           Fox.Types.Token       (Term, Token(..))
