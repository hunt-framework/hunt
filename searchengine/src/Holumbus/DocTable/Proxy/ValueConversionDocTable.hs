module Holumbus.DocTable.Proxy.ValueConversionDocTable
where

import           Prelude                          hiding (null, filter, map, lookup)
import           Control.Arrow                    (second)

import qualified Holumbus.Index.Common.DocIdMap   as DM
import           Holumbus.DocTable.DocTable

-- ------------------------------------------------------------

-- | A value conversion DocTable wich can be used as a proxy with two conversion functions (bijection).
empty :: (a -> v) -> (v -> a) -> DocTable i a -> DocTable (DocTable i a) v
empty from to i =
    Dt
    {
      _null         = null i
    , _size         = size i
    , _lookup       = fmap from . lookup i
    , _lookupByURI  = lookupByURI i
    , _union        = cv . union i . impl
    , _disjoint     = disjoint i . impl
    , _insert       = second cv . insert i . to
    , _update       = \did e -> cv $ update i did (to e)
    , _delete       = cv . delete i
    , _difference   = cv . flip difference i
    , _map          = \f -> cv $ map (to . f . from) i
    , _filter       = \f -> cv $ filter (f . from) i
    , _toMap        = DM.map from (toMap i)
    , _mapKeys      = \f -> cv $ mapKeys f i
    , _impl         = i
    }
    where
      cv = empty from to
