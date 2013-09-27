module Holumbus.Index.Proxy.CachedIndex
where

{--
import           Prelude                           hiding (lookup, subtract)
import qualified Prelude                           as P

import           Control.Arrow                     (second)

import qualified Data.IntSet                       as IS

import           Holumbus.Index.Common.DocIdMap    (DocIdSet)
import qualified Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Index.Index

-- ------------------------------------------------------------
newtype CachedIndex impl = CachedIx (DocIdSet, impl) 

instance Index CachedIndex where
    type IKey PrefixTree v = PT.Key
 
    insert k v (CachedIx (c,i)                 
      = CachedIx (c, insert k v i)

    batchDelete ks pt      = foldr (\k i -> delete k i) pt ks
    delete                 = 

    empty                  = PT.empty
    fromList               = PT.fromList
    toList                 = PT.toList
    search _               = PT.prefixFindWithKey
    unionWith              = PT.unionWith 


realIx    = deleteDocs keySet i -- the doctable with docs deleted
new       = newIndex keySet
deleteIds = IS.fold Occ.delete

-- | An index with an empty cache.
empty :: Index it v i -> Index it v i
empty = newIndex IS.empty


-- | A cached Index. Documents are not deleted right away but stored in a separate set.
--   This way we do not have to walk the whole index.
--   The deleted items are removed from the results of lookups.
--   Trade-off between lookup (every time) and delete (once) performance.
--   The set of deleted items should be merged before it gets too big.
newIndex :: DocIdSet -> Index it v i -> Index it v i
newIndex keySet i =
    Ix {
      _unique                        = unique i   -- XXX: inaccurate
    , _contexts                      = contexts i -- XXX: inaccurate
    , _size                          = map (second (flip deleteIds keySet)) . size i
    , _lookup                        = \it c w -> map (second (flip deleteIds keySet)) $ lookup it i c w
    , _insert                        = \c w o -> new $ insert c w o i
    , _delete                        = \c w o -> new $ delete c w o i
    , _deleteDocs                    = \s -> newIndex (IS.union keySet s) i
    -- the following functions merge the keySet before doing anything
    , _merge                         = new . merge realIx
    , _subtract                      = new . subtract realIx
    {-
    , _splitByContexts               = map new . splitByContexts realIx
    , _splitByDocuments              = map new . splitByDocuments realIx
    , _splitByWords                  = map new . splitByWords realIx
    , _mapDocIds                     = \f -> new $ mapDocIds f realIx
    -}
    , _toList                        = toList realIx
    , _impl                          = impl realIx
    }
    where
-}
