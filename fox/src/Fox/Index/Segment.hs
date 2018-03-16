module Fox.Index.Segment where

import qualified Fox.Schema as Schema
import qualified Fox.Types.Generation as Generation

-- | A description of a part of an index. A 'Segment' can be queried
-- for data.
data Segment
  = Segment {
        segGeneration :: !Generation.Generation
      , segNumDocs    :: !Int
      , segFields     :: !Schema.FieldOrds
      }
