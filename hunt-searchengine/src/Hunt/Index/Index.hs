{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}

module Hunt.Index.Index
where

import           GHC.Exts                       (Constraint)

import           Control.DeepSeq
import qualified Data.IntSet                    as IS
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdMap
import           Hunt.Common.DocId

-- ----------------------------------------------------------------------------

class Index i where
    type IKey      i v :: *

    type IVal      i v :: *
    type IVal      i v = v

    type ICon      i v :: Constraint
    type ICon      i v =  NFData v

    -- | General lookup function.
    search       :: (ICon i v, Monad m) => TextSearchOp -> IKey i v -> i v -> m [(IKey i v, IVal i v)]

    -- | Search within a range.
    lookupRange  :: (ICon i v, Monad m) => IKey i v -> IKey i v -> i v -> m [(IKey i v, IVal i v)]

    -- | Insert occurrences.
    insert       :: (ICon i v, Monad m) => IKey i v -> IVal i v -> i v -> m (i v)

    -- | Delete as batch job
    batchDelete  :: (ICon i v, Monad m) => DocIdSet -> i v -> m (i v)

    -- | Delete occurrences.
    delete       :: (ICon i v, Monad m) => DocId -> i v -> m (i v)
    delete k i   = batchDelete (IS.singleton k) i

    -- | Empty Index
    empty        :: (ICon i v) => i v

    -- | Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations
    toList       :: (ICon i v, Monad m) => i v -> m [(IKey i v, IVal i v)]

    -- | Make index from list
    fromList     :: (ICon i v, Monad m) => [(IKey i v, IVal i v)] -> m (i v)

    -- | Support for index value transformations
    unionWith    :: (ICon i v, Monad m)
                 => (IVal i v -> IVal i v -> IVal i v)
                 -> i v -> i v -> m (i v)

    -- TODO: non-rigid map
    map          :: (ICon i v, Monad m)
                 => (IVal i v -> IVal i v)
                 -> i v -> m (i v)

    -- XXX: maybe less generic with just list?
    keys         :: (ICon i v, Monad m)
                 => i v -> m [IKey i v]
