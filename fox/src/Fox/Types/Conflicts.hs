module Fox.Types.Conflicts where

import           Fox.Types.DocDesc
import           Fox.Types.SegmentId

-- | A @Conflict@ occurs if two transactions changed the same
-- @Segment@s.
data Conflict = ConflictDelete SegmentId
              | ConflictFields FieldName FieldType FieldType
              deriving (Show)

type Commit a = Either [Conflict] a

fieldTyConflict :: FieldName -> FieldType -> FieldType -> Conflict
fieldTyConflict fieldName fieldTy fieldTy' =
  ConflictFields fieldName fieldTy fieldTy'
