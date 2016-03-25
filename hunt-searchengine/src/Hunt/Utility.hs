{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE EmptyDataDecls #-}

-- ----------------------------------------------------------------------------
{- |
  General utitlity functions.
-}
-- ----------------------------------------------------------------------------

module Hunt.Utility
  (
    -- * Function Composition
    (.::), (.:::)

    -- * Safe / Total Functions
  , head'

    -- * Strict Functions
  , foldM'

    -- * Monadic Functions
  , foldlWithKeyM, foldrWithKeyM
  , whenM

    -- * Maybe Helper
  , catMaybesSet

    -- * Either Helper
  , isLeft, isRight
  , fromLeft, fromRight

    -- * List Helper
  , unbox, unboxM, isSingleton
  , split
  , partitionListByLength, partitionListByCount
  , descending

    -- * String Helper
  , strip, stripl, stripr, stripWith
  , escape

    -- * Text Helper
  , showText

    -- * Aeson Helper
  , object', (.=?), (.==), (.\.)

  -- * Types
  , TypeDummy
  )
where

import           Control.Monad (when)

import           Data.Aeson hiding (decode)
import           Data.Aeson.Types
import           Data.Char
import qualified Data.Foldable as FB
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Numeric (showHex)

-- ------------------------------------------------------------

-- | A dummy type without constructor.
data TypeDummy

-- ------------------------------------------------------------

showText :: Show a => a -> Text
showText = T.pack . show

-- | The boob operator.
(.::) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.::) = (.).(.)

-- | The Total Recall operator.
(.:::) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:::) = (.).(.).(.)

-- | Safe 'head'.
head' :: [a] -> Maybe a
head' xs = if null xs then Nothing else Just $ head xs

-- | 'Data.Maybe.catMaybes' on a 'Set' instead of a List.
catMaybesSet :: Ord a => Set (Maybe a) -> [a]
catMaybesSet = S.toList . S.map fromJust . S.delete Nothing

-- | Test if 'Either' is a 'Left' value.
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Test if 'Either' is a 'Right' value.
isRight :: Either a b -> Bool
isRight = not . isLeft

-- | Unwraps a 'Left' value - fails on 'Right'.
fromLeft :: Either a b -> a
fromLeft = either id (error "Hunt.Utility.fromLeft: Right")

-- | Unwraps a 'Right' value - fails on 'Left'.
fromRight :: Either a b -> b
fromRight = either (error "Hunt.Utility.fromRight: Left") id

-- | Unbox a singleton.
--
--   /Note/: This fails if the list is not a singleton.
unbox :: [a] -> a
unbox [e] = e
unbox _   = error "unbox: []"

-- | Unbox a singleton in a safe way with 'Maybe'.
unboxM :: [a] -> Maybe a
unboxM [e] = Just e
unboxM _   = Nothing

-- | Test if the list contains a single element.
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

-- | Split a string into seperate strings at a specific character sequence.
split :: Eq a => [a] -> [a] -> [[a]]
split _ []       = [[]]
split at w@(x:xs) = maybe ((x:r):rs) ((:) [] . split at) (L.stripPrefix at w)
                    where (r:rs) = split at xs

-- | Removes leading and trailing whitespace from a string.
strip :: String -> String
strip = stripWith isSpace

-- | Removes leading whitespace from a string.
stripl :: String -> String
stripl = dropWhile isSpace

-- | Removes trailing whitespace from a string.
stripr :: String -> String
stripr = reverse . dropWhile isSpace . reverse

-- | Strip leading and trailing elements matching a predicate.
stripWith :: (a -> Bool) -> [a] -> [a]
stripWith f = reverse . dropWhile f . reverse . dropWhile f

-- | partition the list of input data into a list of input data lists of
--   approximately the same specified length
partitionListByLength :: Int -> [a] -> [[a]]
partitionListByLength _ [] = []
partitionListByLength count l  = take count l : partitionListByLength count (drop count l)

-- | partition the list of input data into a list of a specified number of input data lists with
--   approximately the same length
partitionListByCount :: Int -> [a] -> [[a]]
partitionListByCount = partition
  where
  partition 0 _ = []
  partition sublists l
    = let next = (length l `div` sublists)
      in  if next == 0  then [l]
                        else take next l : partition (sublists -1) (drop next l)

-- | To use with 'sortBy'.
descending :: Ord a => a -> a -> Ordering
descending = flip compare

-- | Escapes non-alphanumeric or space characters in a String
escape :: String -> String
escape []     = []
escape (c:cs)
  = if isAlphaNum c || isSpace c
      then c : escape cs
      else '%' : showHex (fromEnum c) "" ++ escape cs

-- | Strict version of 'foldM'.
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ acc [] = return acc
foldM' f acc (x:xs) = do
  !acc' <- f acc x
  foldM' f acc' xs

-- | 'FB.foldrM' for 'Map' with key.
foldrWithKeyM :: (Monad m) => (k -> a -> b -> m b) -> b -> Map k a -> m b
foldrWithKeyM f b = FB.foldrM (uncurry f) b . M.toList

-- | 'FB.foldlM' for 'Map' with key.
foldlWithKeyM :: (Monad m) => (b -> k -> a -> m b) -> b -> Map k a -> m b
foldlWithKeyM f b = FB.foldlM f' b . M.toList
  where f' a = uncurry (f a)

-- | Monadic 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM bm f = bm >>= \b -> when b f

-- ------------------------------------------------------------
-- Aeson helper
-- ------------------------------------------------------------

-- | Like 'Data.Aeson.object' to use with '.=?', '.==' and '.\.'.
--   This allows omitting (default) values in 'ToJSON' instances.
--
-- > toJSON (Example maybe list float) = object' $
-- >   [ "maybe" .=? maybe .\. isNothing
-- >   , "list"  .=? list  .\. null
-- >   , "float" .=? float .\. (== 1.0)
-- >   ]
object' :: [[Pair]] -> Value
object' = object . Prelude.concat

-- | Like '.=' but allows omitting of (default) values in 'ToJSON' instances.
--
--   See 'object''.
(.=?) :: ToJSON a => Text -> (a, a -> Bool) -> [Pair]
name .=? (value, cond) = if cond value then [] else [ name .= value ]

-- | '.==' for 'object''.
--
--   See 'object''.
(.==) :: ToJSON a => Text -> a -> [Pair]
name .== value = [ name .= value ]

-- | Use with '.=?' to specify which value to omit.
--
--   See 'object''.
(.\.) :: ToJSON a => a -> (a -> Bool) -> (a, a -> Bool)
v .\. c = (v,c)
infixl 8 .=?

-- ------------------------------------------------------------
