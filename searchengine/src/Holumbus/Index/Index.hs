{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}

module Holumbus.Index.Index
where

import           GHC.Exts                       (Constraint)

import           Control.DeepSeq
import qualified Data.IntSet                    as IS
import           Holumbus.Common

-- ----------------------------------------------------------------------------

class Index i where
    type IKey      i v :: *

    type IVal      i v :: *
    type IVal      i v = v

    type ICon      i v :: Constraint
    type ICon      i v =  NFData v

    -- | General lookup function.
    search       :: ICon i v => TextSearchOp -> IKey i v -> i v -> [(IKey i v, IVal i v)]

    -- | Search within a range.
    lookupRange  :: ICon i v => IKey i v -> IKey i v -> i v -> [(IKey i v, IVal i v)]

    -- | Insert occurrences.
    insert       :: ICon i v => IKey i v -> IVal i v -> i v -> i v

    -- | Delete as batch job
    batchDelete  :: ICon i v => DocIdSet -> i v -> i v

    -- | Delete occurrences.
    delete       :: ICon i v => DocId -> i v -> i v
    delete k i   = batchDelete (IS.singleton k) i

    -- | Empty Index
    empty        :: ICon i v => i v

    -- | Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations
    toList       :: ICon i v => i v -> [(IKey i v, IVal i v)]

    -- | Make index from list
    fromList     :: ICon i v => [(IKey i v, IVal i v)] -> i v

    -- | Support for index value transformations
    unionWith    :: ICon i v
                 => (IVal i v -> IVal i v -> IVal i v)
                 -> i v -> i v -> i v

    -- TODO: non-rigid map
    map          :: ICon i v
                 => (IVal i v -> IVal i v)
                 -> i v -> i v

    -- XXX: maybe less generic with just list?
    keys         :: ICon i v
                 => i v -> [IKey i v]
