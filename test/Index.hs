module Index
where

import           Control.Arrow (first, second)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Text     (Text)
import qualified Data.Text     as Text

data Index k v = Ix
    { _insert :: k -> v -> Index k v
    , _delete :: k      -> Index k v
    , _search :: k      -> Maybe v
    , _toList :: [(k, v)]
    }

-- ----------------------------------------
--
-- the external interface

insert :: k -> v -> Index k v -> Index k v
insert k v ix = (_insert ix) k v

delete :: k -> Index k v -> Index k v
delete k ix = (_delete ix) k

search :: k -> Index k v -> Maybe v
search k ix = (_search ix) k

toList :: Index k v -> [(k, v)]
toList ix = _toList ix

-- ----------------------------------------

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

emptyMapIndex :: Ord k => Index k v
emptyMapIndex = newMapIndex Map.empty

-- ----------------------------------------

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

emptyListIndex :: Eq k => Index k v
emptyListIndex = newListIndex []

-- ----------------------------------------

newConvValueIndex :: (a -> v) -> (v -> a) -> Index k a -> Index k v
newConvValueIndex from to ix =
    Ix
    { _insert = \ k v -> cv $ insert k (to v) ix
    , _delete = \ k   -> cv $ delete k        ix
    , _search = \ k   -> fmap from $ search k ix
    , _toList = fmap (second from) $ toList   ix
    }
    where
      cv = newConvValueIndex from to

-- ----------------------------------------

newConvKeyIndex :: (k1 -> k) -> (k -> k1) -> Index k1 v -> Index k v
newConvKeyIndex from to ix =
    Ix
    { _insert = \ k v -> cv $ insert (to k) v ix
    , _delete = \ k   -> cv $ delete (to k)   ix
    , _search = \ k   ->      search (to k)   ix
    , _toList = fmap (first from) $ toList    ix
    }
    where
      cv = newConvKeyIndex from to

-- ----------------------------------------

newNestedIndex :: Index k2 v -> Index k1 (Index k2 v) -> Index (k1, k2) v
newNestedIndex empty2 ix =
    Ix
    { _insert = ins
    , _delete = del
    , _search = \ (k1, k2) -> search k1 ix >>= search k2
    , _toList = distribute $ map (second toList) $ toList ix
    }
    where
      new = newNestedIndex empty2

      ins (k1, k2) v =
          new $ insert k1 v1 ix
          where
            v1 = insert k2 v $ fromMaybe empty2 $ search k1 ix

      del (k1, k2) =
          new $ case search k1 ix of
                  Nothing -> ix
                  Just x  -> insert k1 (delete k2 x) ix

      distribute =
         concatMap (\ (k1, m) -> map (first (\ k2 -> (k1, k2))) m)

-- ----------------------------------------

type Context = Char
type Word = String
type ContextWordIndex v = Index (Context, Word) v

emptyContextWordIndex :: ContextWordIndex v
emptyContextWordIndex = newNestedIndex emptyListIndex emptyMapIndex

l2 :: [((Context,Word), Integer)]
l2 = map (first (\x -> (head x, tail x))) l1

cx :: ContextWordIndex Integer
cx = foldr (uncurry insert) emptyContextWordIndex $ l2

-- ----------------------------------------

l1 :: [(String, Integer)]
l1 = [("aaa", 111), ("abc",123), ("xxx", 777), ("xyz", 789), ("mmm", 444), ("mno", 456)]

ix1 :: Index String Integer
ix1 = newMapIndex $ Map.fromList l1

ix2 :: Index String Integer
ix2 = newListIndex l1

ix3 :: Index String String
ix3 = newConvValueIndex show read ix1

ix4 :: Index Text String
ix4 = newConvKeyIndex Text.pack Text.unpack ix3
