-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Utility
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Small utility functions which are probably useful somewhere else, too.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Utility where

import           Data.Binary
import qualified Data.ByteString.Lazy   as B
import           Data.Char
import qualified Data.List              as L
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Maybe             (fromJust)

import           Numeric                (showHex)

import           System.IO

-- ------------------------------------------------------------

-- | The boob operator.
(.::) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.::) = (.).(.)

-- | The Total Recall operator.
(.:::) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:::) = (.).(.).(.)

-- | Data.Maybe.catMaybes on a Set instead of a List.
catMaybesSet :: Ord a => Set (Maybe a) -> Set a
catMaybesSet = S.map fromJust . S.delete Nothing

-- | Split a string into seperate strings at a specific character sequence.
split :: Eq a => [a] -> [a] -> [[a]]
split _ []       = [[]]
split at w@(x:xs) = maybe ((x:r):rs) ((:) [] . split at) (L.stripPrefix at w)
                    where (r:rs) = split at xs

-- | Join with a seperating character sequence.
join :: Eq a => [a] -> [[a]] -> [a]
join = L.intercalate

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

-- | found on the haskell cafe mailing list
--   (<http://www.haskell.org/pipermail/haskell-cafe/2008-April/041970.html>).
--   Depends on bytestring >= 0.9.0.4 (?)
strictDecodeFile :: Binary a => FilePath -> IO a
strictDecodeFile f  =
    withBinaryFile f ReadMode
            $ \h -> do c <- B.hGetContents h
                       return $! decode c

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

-- | Escapes non-alphanumeric or space characters in a String
escape :: String -> String
escape []     = []
escape (c:cs)
  = if isAlphaNum c || isSpace c
      then c : escape cs
      else '%' : showHex (fromEnum c) "" ++ escape cs

-- ------------------------------------------------------------
