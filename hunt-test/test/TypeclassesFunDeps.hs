{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies , FlexibleInstances, FlexibleContexts #-}
module TypeclassesFunDeps where

import           Data.Map (Map)
import qualified Data.Map as M

-- ----------------------------------------------------------------------------

class MapLike i k v | i -> k v where
  put :: k -> v -> i -> i
  get :: k -> i -> Maybe v

instance Ord k => MapLike (Map k v) k v where
  put = M.insert
  get = M.lookup

-- ----------------------------------------------------------------------------

putPrefix :: (MapLike i k String) => k -> String -> i -> i
putPrefix k str = put k $ "this is a string: " ++ str

-- ----------------------------------------------------------------------------

k1, v1 :: String
(k1, v1) = ("key", "value")

t1 = put       k1 v1 M.empty
t2 = putPrefix k1 v1 M.empty

-- without 'FunctionalDependencies', this requires 'NoMonomorphismRestriction'
-- additionally it will infer the following type signatures
-- t1, t2 :: MapLike (Map k v) String String => Map k v
-- and cannot produce a value since the result types k and v are ambiguous

-- with 'FunctionalDependencies' (MapLike i k v | i -> k v)
-- the compiler can infer the desired types
-- because the implementation determines the types of k and v
-- which would otherwise be a simple relation
-- t1, t2 :: Map String String