{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.Index.Index
where

import           Holumbus.Index.Common          (Textual,DocId,DocIdMap)
import           Holumbus.Index.Common.DocIdMap (DocIdSet)
import qualified Data.IntSet                    as IS 
import           GHC.Exts                       (Constraint)

-- ----------------------------------------------------------------------------

class Index i where
    type IKey    i v :: *

    type IVal    i v :: *
    type IVal    i v = v
    
    type IToL    i v :: *
    type IToL    i v = [(IKey i v, IVal i v)]

    type IType   i v :: *
    type IType   i v = Textual
 
    type ICon    i v :: Constraint
    type ICon    i v =  ()

    -- | The occurrences for every word. A potentially expensive operation.
    -- size       :: i -> Context -> RawResult

    -- | General lookup function.
    search       :: ICon i v => IType i v -> IKey i v -> i v -> IToL i v
    
    -- | xxx TODO remove this later
    lookup       :: ICon i v => IType i v -> IKey i v -> i v -> IToL i v
    lookup       = search

    -- | Insert occurrences.
    insert       :: ICon i v => IKey i v -> IVal i v -> i v -> i v

    -- | Delete as batch job
    batchDelete  :: ICon i v => DocIdSet -> i v -> i v

    -- | Delete occurrences.
    delete       :: ICon i v => DocId -> i v -> i v
    delete k i   = batchDelete (IS.fromList [k]) i

    -- | Empty Index
    empty        :: ICon i v => i v

    -- | Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations
    toList       :: ICon i v => i v -> IToL i v

    -- | Make index from list 
    fromList     :: ICon i v => IToL i v -> i v

    -- | Support for index value transformations
    unionWith    :: ICon i v 
                 => (IVal i v -> IVal i v -> IVal i v)
                 -> i v -> i v -> i v



-- ----------------------------------------------------------------------------
{-
-- | Create an Index from a list of context, word, occurrences triples.
--   The first argument should be (a specific implementation of) an empty Index.
fromList              :: Index it v i -> [(Context, Word, v)] -> Index it v i
fromList              = foldl (\i (c,w,o) -> insert c w o i)

-- ----------------------------------------------------------------------------
-}

{------------------------------------------
 - functions from old type class impl. 
 - not sure if we still need them
 - moved them here to keep the actual used code clearer
 -}
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


