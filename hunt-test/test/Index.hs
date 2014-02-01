{-# LANGUAGE BangPatterns #-}

module Index
where

import           Control.Arrow (first, second)
import           Control.DeepSeq

import           GHC.AssertNF

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

import qualified Data.StringMap       as SM

import           Data.Maybe
import           Data.Text     (Text)
import qualified Data.Text     as Text

data Index ix k v = Ix
    { _insert :: k -> v -> Index ix k v
    , _delete :: k      -> Index ix k v
    , _merge  :: Index ix k v -> Index ix k v
    , _search :: k      -> Maybe v
    , _toList :: [(k, v)]
    , _impl   :: ! ix
    }

-- ----------------------------------------
--
-- the external interface

insert :: k -> v -> Index ix k v -> Index ix k v
insert k v ix = (_insert ix) k v

delete :: k -> Index ix k v -> Index ix k v
delete k ix = (_delete ix) k

merge :: Index ix k v -> Index ix k v -> Index ix k v
merge ix1 ix2 = (_merge ix1) ix2

search :: k -> Index ix k v -> Maybe v
search k ix = (_search ix) k

toList :: Index ix k v -> [(k, v)]
toList = _toList

impl :: Index ix k v -> ix
impl = _impl

-- ----------------------------------------

newMapIndex :: Ord k => Map k v -> Index (Map k v) k v
newMapIndex m =
    Ix
    { _insert = \ k v -> newMapIndex $
                         Map.insert k v m
    , _delete = \ k   -> newMapIndex $
                         Map.delete k m
    , _merge  = \ ix2 -> newMapIndex $
                         Map.union (_impl ix2) m
    , _search = \ k   -> Map.lookup k m
    , _toList = Map.toList m
    , _impl   = m
    }

emptyMapIndex :: Ord k => Index (Map k v) k v
emptyMapIndex = newMapIndex Map.empty

-- ----------------------------------------

newPTIndex :: SM.StringMap v -> Index (SM.StringMap v) String v
newPTIndex m =
    Ix
    { _insert = \ k v -> newPTIndex $
                         SM.insert k v m
    , _delete = \ k   -> newPTIndex $
                         SM.delete k m
    , _merge  = \ ix2 -> newPTIndex $
                         SM.union (_impl ix2) m
    , _search = \ k   -> SM.lookup k m
    , _toList = SM.toList m
    , _impl   = m
    }

emptyPTIndex :: Index (SM.StringMap v) String v
emptyPTIndex = newPTIndex SM.empty

-- ----------------------------------------

newtype AssocList k v = AL [(k, v)]

newListIndex :: Eq k => AssocList k v -> Index (AssocList k v) k v
newListIndex m@(AL xs) =
    Ix
    { _insert = \ k v -> newListIndex $
                         AL $ (k, v) : xs
    , _delete = \ k   -> newListIndex $
                         AL $ filter ((/= k) . fst) xs
    , _merge  = \ ix2 -> newListIndex $
                         AL $ let AL xs2 = _impl ix2 in xs2 ++ xs 
    , _search = \ k   -> lookup k xs
    , _toList = xs
    , _impl   = m
    }

emptyListIndex :: Eq k => Index (AssocList k v) k v
emptyListIndex = newListIndex $ AL []

-- ----------------------------------------

newConvValueIndex :: (a -> v) -> (v -> a) -> Index ix1 k a -> Index (Index ix1 k a) k v
newConvValueIndex from to ix =
    Ix
    { _insert = \ k v -> cv $ insert k (to v)   ix
    , _delete = \ k   -> cv $ delete k          ix
    , _merge  = \ ix2 -> cv $ merge (_impl ix2) ix
    , _search = \ k   -> fmap from $ search k   ix
    , _toList = fmap (second from) $ toList     ix
    , _impl   = ix
    }
    where
      cv = newConvValueIndex from to

-- ----------------------------------------

newConvKeyIndex :: (k1 -> k) -> (k -> k1) -> Index ix1 k1 v -> Index (Index ix1 k1 v) k v
newConvKeyIndex from to ix =
    Ix
    { _insert = \ k v -> cv $ insert (to k)    v ix
    , _delete = \ k   -> cv $ delete (to k)      ix
    , _merge  = \ ix2 -> cv $ merge  (_impl ix2) ix
    , _search = \ k   ->      search (to k)      ix
    , _toList = fmap (first from) $ toList       ix
    , _impl   = ix
    }
    where
      cv = newConvKeyIndex from to

-- ----------------------------------------

newNestedIndex :: Index ix2 k2 v -> Index ix1 k1 (Index ix2 k2 v) -> Index (Index ix1 k1 (Index ix2 k2 v)) (k1, k2) v
newNestedIndex empty2 ix =
    Ix
    { _insert = ins
    , _delete = del
    , _merge  = mrg
    , _search = \ (k1, k2) -> search k1 ix >>= search k2
    , _toList = distribute $ map (second toList) $ toList ix
    , _impl   = ix
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
      mrg ix2 =
          new $ merge (_impl ix2) ix

      distribute =
         concatMap (\ (k1, m) -> map (first (\ k2 -> (k1, k2))) m)

-- ----------------------------------------

type Context = Char
type Word = String

type MapIndex                 k v = Index (Map k v) k v
type AssocListIndex           k v = Index (AssocList k v) k v
type ConvValMapIndex       v1 k v = Index (MapIndex k v1) k v
type ConvKeyValMapIndex k1 v1 k v = Index (ConvValMapIndex v1 k1 v) k v
type PTIndex                    v = Index (SM.StringMap v) String v

-- signature computed with ghci
type ContextWordIndex' v = Index (Index (Map Char (Index (AssocList String v) String v)) Char (Index (AssocList String v) String v)) (Char, String) v

type ContextWordIndex  v = Index (MapIndex Char (AssocListIndex String v)) (Char, String) v

-- signature computed with ghci
-- emptyContextWordIndex :: Index (Index (Map Char (Index (AssocList String v) String v)) Char (Index (AssocList String v) String v)) (Char, String) v

emptyContextWordIndex :: ContextWordIndex v
emptyContextWordIndex = newNestedIndex emptyListIndex emptyMapIndex

l2 :: [((Context, Word), Integer)]
l2 = map (first (\x -> (head x, tail x))) l1

cx :: ContextWordIndex Integer
cx = foldr (uncurry insert) emptyContextWordIndex $ l2

-- ----------------------------------------

l1 :: [(String, Integer)]
l1 = [("aaa", 111), ("abc",123), ("xxx", 777), ("xyz", 789), ("mmm", 444), ("mno", 456)]

l1' :: [(String, String)]
l1' = map (second show) l1

x1 :: MapIndex String Integer
-- x1 :: Index (Map String Integer) String Integer
x1 = newMapIndex $! Map.fromList l1

x2 :: AssocListIndex String Integer
-- x2 :: Index (AssocList String Integer) String Integer
x2 = newListIndex $ AL l1

x3 :: ConvValMapIndex Integer String String
-- x3 :: Index (Index (Map String Integer) String Integer) String String
x3 = newConvValueIndex show read x1

x4 :: ConvKeyValMapIndex String Integer Text String
-- x4 :: Index (Index (Index (Map String Integer) String Integer) String String) Text String
x4 = newConvKeyIndex Text.pack Text.unpack x3

x5 :: PTIndex Integer
x5 = newPTIndex $ SM.fromList l1

x6 :: PTIndex String
x6 = foldr (uncurry insert') emptyPTIndex $ l1'
    where
      insert' x y = insert x $!! y

t1 :: Index a k v -> IO ()
t1 x = (return $! _impl x) >>= assertNF
