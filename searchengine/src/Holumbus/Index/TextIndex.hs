module Holumbus.Index.TextIndex
( module Holumbus.Index.Index
, TextIndex
, insertPosition
, deletePosition)
where

import           Holumbus.Index.Common             (Context, DocId, Occurrences,
                                                    Position, Textual, Word)
import qualified Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Index.Index

-- ----------------------------------------------------------------------------

-- | TextIndex with implementation type parameter. Uses 'Textual' and 'Occurrences' as index and value type.
type TextIndex i        = Index Textual Occurrences i

-- | Insert a position for a single document.
insertPosition          :: Context -> Word -> DocId -> Position -> TextIndex i -> TextIndex i
insertPosition c w d p  = insert c w (Occ.singleton d p)

-- | Delete a position for a single document.
deletePosition          :: Context -> Word -> DocId -> Position -> TextIndex i -> TextIndex i
deletePosition c w d p  = delete c w (Occ.singleton d p)
