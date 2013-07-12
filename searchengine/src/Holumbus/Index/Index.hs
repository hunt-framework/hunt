module Holumbus.Index.Index
where

import           Data.Set                       (Set)
import           Data.Text                      (Text)

import           Holumbus.Index.Common          (Context, DocId, Occurrences,
                                                 Position, RawResult, Word)

-- ----------------------------------------------------------------------------
--
-- external interface

sizeWords             :: Index i -> Int
sizeWords             = _sizeWords

-- | Returns a list of all contexts avaliable in the index.
contexts              :: Index i -> [Context]
contexts              = _contexts

-- | Returns the occurrences for every word. A potentially expensive operation.
allWords              :: Index i -> Context -> RawResult
allWords              = _allWords

-- | Searches for words beginning with the prefix in a given context (case-sensitive).
prefixCase            :: Index i -> Context -> Text -> RawResult
prefixCase            = _prefixCase

-- | Searches for words beginning with the prefix in a given context (case-insensitive).
prefixNoCase          :: Index i -> Context -> Text -> RawResult
prefixNoCase          = _prefixNoCase

-- | Searches for and exact word in a given context (case-sensitive).
lookupCase            :: Index i -> Context -> Text -> RawResult
lookupCase            = _lookupCase

-- | Searches for and exact word in a given context (case-insensitive).
lookupNoCase          :: Index i -> Context -> Text -> RawResult
lookupNoCase          = _lookupNoCase

-- | Insert occurrences.
insertOccurrences     :: Context -> Word -> Occurrences -> Index i -> Index i
insertOccurrences     = \c w o i -> _insertOccurrences i c w o

-- | Delete occurrences.
deleteOccurrences     :: Context -> Word -> Occurrences -> Index i -> Index i
deleteOccurrences     = \c w o i -> _deleteOccurrences i c w o

-- | Insert a position for a single document.
insertPosition        :: Context -> Word -> DocId -> Position -> Index i -> Index i
insertPosition        = \c w d p i -> _insertPosition i c w d p
--insertPosition c w d p i      ,, insertOccurrences c w (singletonOccurrence d p) Index

-- | Delete a position for a single document.
deletePosition        :: Context -> Word -> DocId -> Position -> Index i -> Index i
deletePosition        = \c w d p i -> _deletePosition i c w d p
--deletePosition c w d p i      ,, deleteOccurrences c w (singletonOccurrence d p) Index

-- | Delete documents completely (all occurrences).
deleteDocsById        :: Set DocId -> Index i -> Index i
deleteDocsById        = \ds i -> _deleteDocsById i ds

-- | Merges two indexes.
mergeIndexes          :: Index i -> Index i -> Index i
mergeIndexes          = _mergeIndexes

-- | Subtract one index from another.
subtractIndexes       :: Index i -> Index i -> Index i
subtractIndexes       = _subtractIndexes

-- | Splitting an index by its contexts.
splitByContexts       :: Index i -> Int -> [Index i]
splitByContexts       = _splitByContexts

-- | Splitting an index by its documents.
splitByDocuments      :: Index i -> Int -> [Index i]
splitByDocuments      = _splitByDocuments

-- | Splitting an index by its words.
splitByWords          :: Index i -> Int -> [Index i]
splitByWords          = _splitByWords

-- | Update document id's (e.g. for renaming documents). If the function maps two different id's
-- to the same new id, the two sets of word positions will be merged if both old id's are present
-- in the occurrences for a word in a specific context.
updateDocIds          :: (Context -> Word -> DocId -> DocId) -> Index i -> Index i
updateDocIds          = \f i -> _updateDocIds i f

-- | Update document id's with a simple injective editing function.
updateDocIds'         :: (DocId -> DocId) -> Index i -> Index i
updateDocIds'         = \f i -> _updateDocIds i (const . const $ f)

-- | Convert an Index to a list. Can be used for easy conversion between different index
-- implementations
toList                :: Index i -> [(Context, Word, Occurrences)]
toList                = _toList

-- | The index implementation
impl                  :: Index i -> i
impl                  = _impl

-- | Create an Index from a list of context, word, occurrences triples.
--   The first argument should be (a specific implementation of) an empty Index.
fromList              :: Index i -> [(Context, Word, Occurrences)] -> Index i
fromList e            = foldl (\i (c,w,o) -> insertOccurrences c w o i) e


-- ----------------------------------------------------------------------------

data Index i = Ix
    {
    -- | Returns the number of unique words in the index.
      _sizeWords                     :: Int

    -- | Returns a list of all contexts avaliable in the index.
    , _contexts                      :: [Context]

    -- | Returns the occurrences for every word. A potentially expensive operation.
    , _allWords                      :: Context -> RawResult

    -- | Searches for words beginning with the prefix in a given context (case-sensitive).
    , _prefixCase                    :: Context -> Text -> RawResult

    -- | Searches for words beginning with the prefix in a given context (case-insensitive).
    , _prefixNoCase                  :: Context -> Text -> RawResult

    -- | Searches for and exact word in a given context (case-sensitive).
    , _lookupCase                    :: Context -> Text -> RawResult

    -- | Searches for and exact word in a given context (case-insensitive).
    , _lookupNoCase                  :: Context -> Text -> RawResult

    -- | Insert occurrences.
    , _insertOccurrences             :: Context -> Word -> Occurrences -> Index i

    -- | Delete occurrences.
    , _deleteOccurrences             :: Context -> Word -> Occurrences -> Index i

    -- | Insert a position for a single document.
    , _insertPosition                :: Context -> Word -> DocId -> Position -> Index i
    --insertPosition c w d p i      = insertOccurrences c w (singletonOccurrence d p) Index

    -- | Delete a position for a single document.
    , _deletePosition                :: Context -> Word -> DocId -> Position -> Index i
    --deletePosition c w d p i      = deleteOccurrences c w (singletonOccurrence d p) Index

    -- | Delete documents completely (all occurrences).
    , _deleteDocsById                :: Set DocId -> Index i

    -- | Merges two indexes.
    , _mergeIndexes                  :: Index i -> Index i

    -- | Subtract one index from another.
    , _subtractIndexes               :: Index i -> Index i

    -- | Splitting an index by its contexts.
    , _splitByContexts               :: Int -> [Index i]

    -- | Splitting an index by its documents.
    , _splitByDocuments              :: Int -> [Index i]

    -- | Splitting an index by its words.
    , _splitByWords                  :: Int -> [Index i]

    -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
    -- to the same new id, the two sets of word positions will be merged if both old id's are present
    -- in the occurrences for a word in a specific context.
    , _updateDocIds                  :: (Context -> Word -> DocId -> DocId) -> Index i

    -- Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations
    , _toList                        :: [(Context, Word, Occurrences)]

    -- Create an Index from a list. Can be used for easy conversion between different index
    -- implementations. Needs an empty index as first argument

    --, _fromList                      :: [(Context, Word, Occurrences)] -> Index
    --fromList e                    = foldl (\i (c,w,o) -> insertOccurrences c w o i) e

    , _impl                          :: i
    }