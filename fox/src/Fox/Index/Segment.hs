module Fox.Index.Segment where

import qualified Fox.Index.InvertedFile as InvertedFile
import qualified Fox.Index.InvertedFile.TermIndex as TermIndex
import qualified Fox.Schema as Schema
import qualified Fox.Types.Document as Document
import qualified Fox.Types.Generation as Generation
import qualified Fox.Types.Token as Token

import qualified Data.Count as Count

-- | A description of a part of an index. A 'Segment' can be queried
-- for data.
data Segment
  = Segment {
        segGeneration  :: !Generation.Generation
        -- ^ the delete generation this 'Segment'
        -- belongs to.

      , segFields      :: !Schema.FieldOrds
        -- ^ The 'FieldOrd' mapping for this 'Segment'

      , segDocCount    :: !(Count.CountOf Document.Document)
        -- ^ The number of documents this 'Segment'
        -- stores

      , segTermCount   :: !(Count.CountOf Token.Term)
        -- ^ The number of unique terms this 'Segment'
        -- stores

      , segTermIxCount :: !(Count.CountOf Token.Term)
        -- ^ Number of unique terms which that do not
        -- share prefixes (e.g. vwlengthPrefix(x) == 0)

      , segLoadTermIx  :: InvertedFile.IfM TermIndex.TermIndex
        -- ^ An action to load the 'Segment's 'TermIndex'.
      }
