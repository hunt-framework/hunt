module Holumbus.Index.Index
where

import           Data.Set                       (Set)
import           Data.Text                      (Text)

import           Holumbus.Index.Common          (Context, RawResult, Word, DocId)       
-- ----------------------------------------------------------------------------
--
-- external interface

sizeWords             :: Index it v i -> Int
sizeWords             = _sizeWords

-- | Returns a list of all contexts avaliable in the index.
contexts              :: Index it v i -> [Context]
contexts              = _contexts

-- | Returns the occurrences for every word. A potentially expensive operation.
allWords              :: Index it v i -> Context -> RawResult
allWords              = _allWords

lookup                :: it -> Index it v i -> Context -> Word -> RawResult
lookup it t c w       = _lookup t it c w

-- | Insert occurrences.
insert                :: Context -> Word -> v -> Index it v i -> Index it v i
insert                = \c w o i -> _insert i c w o

-- | Delete occurrences.
delete                :: Context -> Word -> v -> Index it v i -> Index it v i
delete                = \c w o i -> _delete i c w o

-- | Delete documents completely (all occurrences).
deleteDocsById        :: Set DocId -> Index it v i -> Index it v i
deleteDocsById        = \ds i -> _deleteDocsById i ds

-- | Merges two indexes.
merge                 :: Index it v i -> Index it v i -> Index it v i
merge                 = _merge

-- | Subtract one index from another.
subtract              :: Index it v i -> Index it v i -> Index it v i
subtract              = _subtract

-- | Splitting an index by its contexts.
splitByContexts       :: Index it v i -> Int -> [Index it v i]
splitByContexts       = _splitByContexts

-- | Splitting an index by its documents.
splitByDocuments      :: Index it v i -> Int -> [Index it v i]
splitByDocuments      = _splitByDocuments

-- | Splitting an index by its words.
splitByWords          :: Index it v i -> Int -> [Index it v i]
splitByWords          = _splitByWords

-- | Update document id's (e.g. for renaming documents). If the function maps two different id's
-- to the same new id, the two sets of word positions will be merged if both old id's are present
-- in the occurrences for a word in a specific context.
updateDocIds          :: (Context -> Word -> DocId -> DocId) -> Index it v i -> Index it v i
updateDocIds          = \f i -> _updateDocIds i f

-- | Update document id's with a simple injective editing function.
updateDocIds'         :: (DocId -> DocId) -> Index it v i -> Index it v i
updateDocIds'         = \f i -> _updateDocIds i (const . const $ f)

-- | Convert an Index to a list. Can be used for easy conversion between different index
-- implementations
toList                :: Index it v i -> [(Context, Word, v)]
toList                = _toList

-- | The index implementation
impl                  :: Index it v i -> i
impl                  = _impl

-- default implementations

-- | Create an Index from a list of context, word, occurrences triples.
--   The first argument should be (a specific implementation of) an empty Index.
fromList              :: Index it v i -> [(Context, Word, v)] -> Index it v i
fromList e            = foldl (\i (c,w,o) -> insert c w o i) e

-- ----------------------------------------------------------------------------
data Index it v i = Ix
    {
    -- | Returns the number of unique words in the index.
      _sizeWords                     :: Int

    -- | Returns a list of all contexts avaliable in the index.
    , _contexts                      :: [Context]

    -- | Returns the occurrences for every word. A potentially expensive operation.
    , _allWords                      :: Context -> RawResult

    -- | general lookup function
    , _lookup                        :: it -> Context -> Text -> RawResult

    -- | Insert occurrences.
    , _insert                        :: Context -> Word -> v -> Index it v i

    -- | Delete occurrences.
    , _delete                        :: Context -> Word -> v -> Index it v i

    -- | Delete documents completely (all occurrences).
    , _deleteDocsById                :: Set DocId -> Index it v i

    -- | Merges two indexes.
    , _merge                         :: Index it v i -> Index it v i

    -- | Subtract one index from another.
    , _subtract                      :: Index it v i -> Index it v i

    -- | Splitting an index by its contexts.
    , _splitByContexts               :: Int -> [Index it v i]

    -- | Splitting an index by its documents.
    , _splitByDocuments              :: Int -> [Index it v i]

    -- | Splitting an index by its words.
    , _splitByWords                  :: Int -> [Index it v i]

    -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
    -- to the same new id, the two sets of word positions will be merged if both old id's are present
    -- in the occurrences for a word in a specific context.
    , _updateDocIds                  :: (Context -> Word -> DocId -> DocId) -> Index it v i

    -- | Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations
    , _toList                        :: [(Context, Word, v)]

    -- | The index implementation.
    , _impl                          :: i
    }
