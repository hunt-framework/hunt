module Fox.Index.Segment where

import qualified Fox.Index.InvertedFile as InvertedFile
import qualified Fox.Schema as Schema
import qualified Fox.Types.Document as Document
import qualified Fox.Types.Generation as Generation

import qualified Data.Count as Count

-- | A description of a part of an index. A 'Segment' can be queried
-- for data.
data Segment
  = Segment {
        segGeneration  :: !Generation.Generation
      , segNumDocs     :: !(Count.CountOf Document.Document)
      , segFields      :: !Schema.FieldOrds
      , segInvFileInfo :: !InvertedFile.InvFileInfo
      }
