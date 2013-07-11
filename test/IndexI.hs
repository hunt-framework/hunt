-- {-# LANGUAGE Rank2Types, ExistentialQuantification #-}

{-
  Index class implemented in the style of
  Scrap your type classes
  ( http://www.haskellforall.com/2012/05/scrap-your-type-classes.html )
-}

module IndexI
where

import           Control.Arrow (first, second)

import           Data.Function (on)
import           Data.List     (sortBy, nubBy)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Text     (Text)
import qualified Data.Text     as Text

-- ----------------------------------------
--
-- the Index "class"

data IndexI ix k v =  -- Index "I"nstance
    IndexI
    { _empty  ::                     ix k v
    , _insert :: k -> v -> ix k v -> ix k v
    , _delete :: k      -> ix k v -> ix k v
    , _search :: k      -> ix k v -> Maybe v
    , _toList ::           ix k v -> [(k, v)]
    }

-- ----------------------------------------
--
-- the "instance" for Map

index'Map :: Ord k => IndexI Map k v
index'Map =
    IndexI
    { _empty  =             Map.empty
    , _insert = \ k v ix -> Map.insert k v ix
    , _delete = \ k   ix -> Map.delete k   ix
    , _search = \ k   ix -> Map.lookup k   ix
    , _toList = \     ix -> Map.toList     ix
    }

empty'Map :: Ord k => Map k v
empty'Map = _empty index'Map

-- ----------------------------------------
--
-- the "instance" for lists

newtype AssocList k v = Assoc [(k, v)]

index'List :: Ord k => IndexI AssocList k v
index'List =
    IndexI
    { _empty  =                     Assoc $ []
    , _insert = \ k v (Assoc xs) -> Assoc $ (k, v) : xs
    , _delete = \ k   (Assoc xs) -> Assoc $ filter ((/= k) . fst) xs
    , _search = \ k   (Assoc xs) -> lookup k xs
    , _toList = \     (Assoc xs) -> sortBy (compare `on` fst) . nubBy ((==) `on` fst) $ xs
    }

empty'List :: Ord k => AssocList k v
empty'List = _empty index'List

-- ----------------------------------------
--

newtype IsoKey ix1 k1 k v = IsoKey (ix1 k1 v)

index'IsoKey :: Iso k k1 -> IndexI ix1 k1 v -> IndexI (IsoKey ix1 k1) k v 
index'IsoKey iso i1 =
    IndexI
    { _empty  =                       IsoKey $ (_empty  i1)
    , _insert = \ k v (IsoKey ix1) -> IsoKey $ (_insert i1) (fw iso $ k) v  ix1
    , _delete = \ k   (IsoKey ix1) -> IsoKey $ (_delete i1) (fw iso $ k)    ix1
    , _search = \ k   (IsoKey ix1) ->          (_search i1) (fw iso $ k)    ix1
    , _toList = \     (IsoKey ix1) -> fmap (first (bw iso)) $
                                               (_toList i1)                 ix1 
    }

-- ----------------------------------------
--

newtype IsoVal ix1 v1 k v = IsoVal (ix1 k v1)

index'IsoVal :: Iso v v1 -> IndexI ix1 k v1 -> IndexI (IsoVal ix1 v1) k v 
index'IsoVal iso i1 =
    IndexI
    { _empty  =                       IsoVal $ (_empty  i1)
    , _insert = \ k v (IsoVal ix1) -> IsoVal $ (_insert i1) k (fw iso $ v) ix1
    , _delete = \ k   (IsoVal ix1) -> IsoVal $ (_delete i1) k              ix1
    , _search = \ k   (IsoVal ix1) -> fmap (bw iso) $
                                               (_search i1) k              ix1
    , _toList = \     (IsoVal ix1) -> fmap (second (bw iso)) $
                                               (_toList i1)                ix1 
    }

-- ----------------------------------------
--
-- isomorphism "class" for conversion of keys and attributes

data Iso a b =
    Iso
    { fw :: a -> b
    , bw :: b -> a
    }

combine :: Iso b c -> Iso a b -> Iso a c
combine iso1 iso2 =
    Iso
    { fw = fw iso1 . fw iso2
    , bw = bw iso2 . bw iso1
    }

invert :: Iso a b -> Iso b a
invert iso = Iso {fw = bw iso, bw = fw iso}

isoToString :: (Show a, Read a) => Iso a String
isoToString = Iso {bw = read, fw = show}

isoFromString :: (Show a, Read a) => Iso String a
isoFromString = invert isoToString

isoStringText :: Iso String Text
isoStringText = Iso {fw = Text.pack, bw = Text.unpack}

isoTextString :: Iso Text String
isoTextString = invert isoStringText

-- ----------------------------------------
--
-- examples

type Context = Char
type Word = String

type WordIntIndex    = Map Word Int
type WordStringIndex = Map Word String

-- an index implemented with an isomorphism on the keys
-- and the assocoated interface

type StringIndex  k v = IsoKey         Map Int  k v
type StringIndexI k v = IndexI (IsoKey Map Int) k v
 
index'StringIntIndex :: StringIndexI String v
index'StringIntIndex = index'IsoKey isoFromString index'Map

empty'StringIntIndex :: StringIndex String v
empty'StringIntIndex = _empty index'StringIntIndex

type StringTextIndex  k v = IsoVal         (IsoKey Map Int)  String k v
type StringTextIndexI k v = IndexI (IndexI (IsoKey Map Int))        k v

index'StringTextIndex :: IndexI (IsoVal (IsoKey Map Int) String) String Text
index'StringTextIndex = index'IsoVal isoTextString index'StringIntIndex

-- ----------------------------------------

l1 :: [(String, Int)]
l1 = [("aaa", 111), ("abc",123), ("xxx", 777), ("xyz", 789), ("mmm", 444), ("mno", 456)]

ix1 :: Map Int String
ix1 = foldr (uncurry (_insert index'Map)) (_empty index'Map)
      $ map (\ (x, y) -> (y,x)) l1

lx1 :: [(Int, String)]
lx1 = (_toList index'Map) ix1

ix2 :: StringIndex String String
ix2 = foldr (uncurry (_insert index'StringIntIndex)) (_empty index'StringIntIndex)
      $ map (\ (x, y) -> (show y, x)) l1

lx2 :: [(String, String)]
lx2 = (_toList index'StringIntIndex) ix2

ix3 :: StringTextIndex String Text
-- ix3 :: IsoVal (IsoKey Map Int) String String Text
-- ix3 :: IsoVal (IsoKey Map Int) String String Text
ix3 = foldr (uncurry (_insert index'StringTextIndex)) (_empty index'StringTextIndex)
      $ map (\ (x, y) -> (show y, Text.pack x)) l1

lx3 :: [(String, Text)]
lx3 = (_toList index'StringTextIndex) ix3

