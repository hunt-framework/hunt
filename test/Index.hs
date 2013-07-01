module Index
where

import           Control.Arrow (first, second)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Text     (Text)
import qualified Data.Text     as Text

data Index k v = Ix
    { _insert :: k -> v -> Index k v
    , _delete :: k      -> Index k v
    , _search :: k      -> Maybe v
    , _toList :: [(k, v)]
    }

insert :: k -> v -> Index k v -> Index k v
insert k v ix = (_insert ix) k v

delete :: k -> Index k v -> Index k v
delete k ix = (_delete ix) k

search :: k -> Index k v -> Maybe v
search k ix = (_search ix) k

toList :: Index k v -> [(k, v)]
toList ix = _toList ix

newMapIndex :: Ord k => Map k v -> Index k v
newMapIndex m =
    Ix
    { _insert = \ k v -> newMapIndex $
                         Map.insert k v m
    , _delete = \ k   -> newMapIndex $
                         Map.delete k m
    , _search = \ k   -> Map.lookup k m
    , _toList = Map.toList m
    }

newListIndex :: Eq a => [(a, v)] -> Index a v
newListIndex xs =
    Ix
    { _insert = \ k v -> newListIndex $
                         (k, v) : xs
    , _delete = \ k   -> newListIndex $
                         filter ((/= k) . fst) xs
    , _search = \ k   -> lookup k xs
    , _toList = xs
    }

convertedValueIndex :: (a -> v) -> (v -> a) -> Index k a -> Index k v
convertedValueIndex from to ix =
    Ix
    { _insert = \ k v -> cv $ insert k (to v) ix
    , _delete = \ k   -> cv $ delete k        ix
    , _search = \ k   -> fmap from $ search k ix
    , _toList = fmap (second from) $ toList   ix
    }
    where
      cv = convertedValueIndex from to

convertedKeyIndex :: (k1 -> k) -> (k -> k1) -> Index k1 v -> Index k v
convertedKeyIndex from to ix =
    Ix
    { _insert = \ k v -> cv $ insert (to k) v ix
    , _delete = \ k   -> cv $ delete (to k)   ix
    , _search = \ k   ->      search (to k)   ix
    , _toList = fmap (first from) $ toList    ix
    }
    where
      cv = convertedKeyIndex from to

l1 :: [(String, Integer)]
l1 = [("abc",123), ("xyz", 789), ("mno", 456)]

ix1 :: Index String Integer
ix1 = newMapIndex $ Map.fromList l1

ix2 :: Index String Integer
ix2 = newListIndex l1

ix3 :: Index String String
ix3 = convertedValueIndex show read ix1

ix4 :: Index Text String
ix4 = convertedKeyIndex Text.pack Text.unpack ix3
