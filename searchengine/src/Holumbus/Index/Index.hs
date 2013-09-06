module Holumbus.Index.Index
where

import           Control.DeepSeq

import           Holumbus.Index.Common          (Context, RawResult, Word)
import           Holumbus.Index.Common.DocIdMap (DocIdSet)

-- ----------------------------------------------------------------------------
--
-- external interface

-- | Returns the number of unique words in the index.
unique                :: Index it v i -> Int
unique                = _unique

-- | Returns a list of all contexts avaliable in the index.
contexts              :: Index it v i -> [Context]
contexts              = _contexts

-- | Returns the occurrences for every word. A potentially expensive operation.
size                  :: Index it v i -> Context -> RawResult
size                  = _size

-- | General lookup function.
lookup                :: it -> Index it v i -> Context -> Word -> RawResult
lookup it t           = _lookup t it

-- | Insert occurrences.
insert                :: Context -> Word -> v -> Index it v i -> Index it v i
insert c w o i        = _insert i c w o

-- | Delete occurrences.
delete                :: Context -> Word -> v -> Index it v i -> Index it v i
delete c w o i        = _delete i c w o

-- | Delete documents completely (all occurrences).
deleteDocs            :: DocIdSet -> Index it v i -> Index it v i
deleteDocs            = flip _deleteDocs

-- | Merges two indexes.
merge                 :: Index it v i -> Index it v i -> Index it v i
merge                 = _merge

-- | Subtract one index from another.
subtract              :: Index it v i -> Index it v i -> Index it v i
subtract              = _subtract

{-
-- | Splitting an index by its contexts.
splitByContexts       :: Index it v i -> Int -> [Index it v i]
splitByContexts       = _splitByContexts

-- | Splitting an index by its documents.
splitByDocuments      :: Index it v i -> Int -> [Index it v i]
splitByDocuments      = _splitByDocuments

-- | Splitting an index by its words.
splitByWords          :: Index it v i -> Int -> [Index it v i]
splitByWords          = _splitByWords

-- | Update document 'DocId's (e.g. for renaming documents). If the function maps two different 'DocId's
-- to the same new 'DocId', the two sets of word positions will be merged if both old 'DocId's are present
-- in the occurrences for a word in a specific context.
mapDocIds             :: (Context -> Word -> DocId -> DocId) -> Index it v i -> Index it v i
mapDocIds             = flip _mapDocIds

-- | Update 'DocId's with a simple injective editing function.
mapDocIds'            :: (DocId -> DocId) -> Index it v i -> Index it v i
mapDocIds' f i        = _mapDocIds i (const . const $ f)
-}

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
fromList              = foldl (\i (c,w,o) -> insert c w o i)

-- ----------------------------------------------------------------------------

-- | The index data type which contains all functions used on the implementation.
--   The type parameters are:
--
--   - @it@: the index type (text, geo, ...)
--
--   - @v@: the values stored (e.g. occurrences in documents)
--
--   - @i@: the implementation
data Index it v i = Ix
    {
    -- | Number of unique words in the index.
      _unique     :: Int

    -- | List of all contexts avaliable in the index.
    , _contexts   :: [Context]

    -- | The occurrences for every word. A potentially expensive operation.
    , _size       :: Context -> RawResult

    -- | General lookup function.
    , _lookup     :: it -> Context -> Word -> RawResult

    -- | Insert occurrences.
    , _insert     :: Context -> Word -> v -> Index it v i

    -- | Delete occurrences.
    , _delete     :: Context -> Word -> v -> Index it v i

    -- | Delete documents completely (all occurrences).
    , _deleteDocs :: DocIdSet -> Index it v i

    -- | Merges two indexes.
    , _merge      :: Index it v i -> Index it v i

    -- | Subtract one index from another.
    , _subtract   :: Index it v i -> Index it v i

    {-
    -- | Splitting an index by its contexts.
    , _splitByContexts               :: Int -> [Index it v i]

    -- | Splitting an index by its documents.
    , _splitByDocuments              :: Int -> [Index it v i]

    -- | Splitting an index by its words.
    , _splitByWords                  :: Int -> [Index it v i]

    -- | Update document id's (e.g. for renaming documents). If the function maps two different id's
    -- to the same new id, the two sets of word positions will be merged if both old id's are present
    -- in the occurrences for a word in a specific context.
    , _mapDocIds                     :: (Context -> Word -> DocId -> DocId) -> Index it v i
    -}

    -- | Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations
    , _toList     :: [(Context, Word, v)]

    -- | The index implementation.
    , _impl       :: i
    }

-- ----------------------------------------------------------------------------

instance NFData (Index it v i) where
    rnf Ix{} = rnf _impl
