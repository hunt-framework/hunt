{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Holumbus.Index.Index
where

import           GHC.Exts                       (Constraint)

import           Control.DeepSeq
import qualified Data.IntSet                    as IS
import           Data.Text                      (Text)
import           Data.Binary

import           Holumbus.Common

-- ----------------------------------------------------------------------------

type IndexImplCon i v = ( Index i
                        , IKey i v ~ Text
                        , IVal i v ~ v
                        , ICon i v
                        -- XXX there is really no way we could
                        -- use this searchop type dynamicly.
                        , ISearchOp i v ~ TextSearchOp
                        , Binary (i v)
                        )

data IndexImpl v 
  = forall i. IndexImplCon i v => IndexImpl { ixImpl :: i v } 

-- | XXX actually implement instance
instance Show (IndexImpl i) where
  show _ = "x" 

-- | XXX actually implement instance
instance Binary (IndexImpl i) where
  put (IndexImpl i) = put i 
  get = undefined


-- | IndexImpl is the Wrapper for external access
--   we set the key to Text here, but allow internal
--   Key in all haskell types. For conversion we have
--   could imagine a normalization proxy implemented
--   with the KeyIndex Proxy
mkIndex      :: IndexImplCon i v  => i v -> IndexImpl v 
mkIndex i    = IndexImpl $! i

class Index i where
    type IKey      i v :: *

    type IVal      i v :: *
    type IVal      i v = v

    type ICon      i v :: Constraint
    type ICon      i v =  NFData v

    type ISearchOp i v :: *
    type ISearchOp i v = TextSearchOp

    -- | General lookup function.
    search       :: ICon i v => ISearchOp i v -> IKey i v -> i v -> [(IKey i v, IVal i v)]

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
