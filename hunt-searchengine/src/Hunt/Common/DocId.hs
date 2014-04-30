{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.DocId
  Copyright  : Copyright (C) 2014 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)

  The document identifier type DocId and the newtype DocId' with Show and ToJSOn instances
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.DocId
where

import           Control.Applicative

import           Data.Aeson
import           Data.Binary          (Binary (..))
import qualified Data.Binary          as B
import           Data.Digest.Murmur64

-- ------------------------------------------------------------

-- | The unique identifier of a document.
type DocId = Int

-- ------------------------------------------------------------

-- | Create the null-identifier.
mkNull :: DocId
mkNull = 0

-- | Create the first identifier.
mkFirst :: DocId
mkFirst = 1

-- | Create a 'DocId' from an 'Integer'.
fromInteger :: Integer -> DocId
fromInteger = fromIntegral

-- ------------------------------------------------------------

{-# INLINE mkNull #-}
{-# INLINE mkFirst #-}
{-# INLINE fromInteger #-}

-- ------------------------------------------------------------

-- the wrapped DocId
-- currently only used for JSON debug output

newtype DocId' = DocId' {unDocId' :: Int}
    deriving (Eq, Ord)

instance Show DocId' where
    show = toHex . unDocId'

instance Binary DocId' where
    put = put . unDocId'
    get = DocId' <$> get

instance ToJSON DocId' where
    toJSON (DocId' i) = toJSON $ toHex i

mkDocId' :: Binary a => a -> DocId'
mkDocId' = DocId' . fromIntegral . asWord64 . hash64 . B.encode

toHex :: Int -> String
toHex !y = "0x" ++ toX 16 "" y
    where
      toX :: Int -> String -> Int -> String
      toX !0 !acc _ = acc
      toX !n !acc x = toX (n - 1) (d : acc) x'
          where
            (!x', !r) = x `divMod` 16
            !d | r < 10    = toEnum (fromEnum '0' + r)
               | otherwise = toEnum (fromEnum 'a' + r - 10)

fromHex :: String -> Maybe Int
fromHex i@('0' : 'x' : xs)
    | length xs == 16
      &&
      all (`elem` "0123456789abcdef") xs
        = Just . read $ i
    | otherwise
        = Nothing

fromHex _
    = Nothing

-- ------------------------------------------------------------
