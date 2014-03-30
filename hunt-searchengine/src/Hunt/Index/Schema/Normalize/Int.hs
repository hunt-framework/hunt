{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  Normalization and validation for integer.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.Schema.Normalize.Int
  ( normalizeToText, denormalizeFromText
  , normalizeToInt, denormalizeFromInt
  , isInt, integer
  )
where

import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Control.Applicative           hiding ((<|>))
import           Hunt.Common.BasicTypes
import           Text.ParserCombinators.Parsec

-- ------------------------------------------------------------
-- Normalize Int to actual Int
-- ------------------------------------------------------------

-- | Normalize an integer to an 'Int'.
normalizeToInt :: Text -> Int
normalizeToInt = getInt

-- | Denormalize an integer.
denormalizeFromInt :: Int -> Text
denormalizeFromInt = T.pack . show

-- ------------------------------------------------------------
-- Normalize Int as Text
-- ------------------------------------------------------------

-- | Normalize a /valid/ integer to a 'Text' representation preserving ordering.
normalizeToText' :: Word -> Word
normalizeToText' i
  = T.concat [pfx, zeros, nr]
  where
  (pfx,nr) = if T.take 1 i == "-"
             then ("0", T.drop 1 i)
             else ("1", i)
  elems    = 20 - T.length nr
  zeros    = T.replicate elems "0"

-- | Normalize an integer to a 'Text' representation preserving ordering.
normalizeToText :: Text -> Text
normalizeToText t
  = if isInt t
    then normalizeToText' t
    else error "normalizeToText: invalid input"

-- | Denormalize a value transformed with 'normalize'.
denormalizeFromText :: Text -> Text
denormalizeFromText i
  = sign raw
  where
  sign = if T.take 1 i == "1" then id else ('-' `T.cons`)
  raw  = T.dropWhile (== '0') $ T.drop 1 i

-- ------------------------------------------------------------
-- Validate int
-- ------------------------------------------------------------

-- | Parse a /valid/ integer.
--
--   /Note/: This will fail on invalid integers.
getInt :: Text -> Int
getInt int = case parse integer "" $ T.unpack int of
  Right pint -> pint
  _          -> error "getInt: invalid input"

-- | Validate if the text represents a valid integer.
--   Needs to be within 'Int' range.
isInt :: Text -> Bool
isInt int = case parse integer "" $ T.unpack int of
  Right pint -> int == (T.pack . show $ pint)
  _          -> False

-- | Parse a simple integer representation.
integer :: Parser Int
integer = rd <$> (plus <|> minus <|> number)
  where
  rd     = read :: String -> Int
  plus   = char '+' *> number
  minus  = (:) <$> char '-' <*> number
  number = many1 digit

-- ------------------------------------------------------------
