{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.Index.Index
where

import           Holumbus.Index.Common          (Context, RawResult, Word)
import           Holumbus.Index.Common.DocIdMap (DocIdSet)

-- ----------------------------------------------------------------------------

class Index i where
    type IValue  i :: *
    type IType   i :: *

    unique     :: i -> Int

    -- | List of all contexts avaliable in the index.
    contexts   :: i -> [Context]

    -- | The occurrences for every word. A potentially expensive operation.
    size       :: i -> Context -> RawResult

    -- XXX: argument order?
    -- | General lookup function.
    lookup     :: IType i -> i -> Context -> Word -> RawResult

    -- | Insert occurrences.
    insert     :: Context -> Word -> IValue i -> i -> i

    -- | Delete occurrences.
    delete     :: Context -> Word -> IValue i -> i -> i

    -- | Delete documents completely (all occurrences).
    deleteDocs :: DocIdSet -> i -> i

    -- | Merges two indexes.
    merge      :: i -> i -> i

    -- | Subtract one index from another.
    subtract   :: i -> i -> i

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
    toList     :: i -> [(Context, Word, IValue i)]

-- ----------------------------------------------------------------------------
{-
-- | Create an Index from a list of context, word, occurrences triples.
--   The first argument should be (a specific implementation of) an empty Index.
fromList              :: Index it v i -> [(Context, Word, v)] -> Index it v i
fromList              = foldl (\i (c,w,o) -> insert c w o i)

-- ----------------------------------------------------------------------------
-}