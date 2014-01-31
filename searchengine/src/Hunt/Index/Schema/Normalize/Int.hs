{-# LANGUAGE OverloadedStrings #-}

module Hunt.Index.Schema.Normalize.Int
where

import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Control.Applicative           hiding ((<|>))
import           Hunt.Common.BasicTypes
import           Text.ParserCombinators.Parsec

-- ----------------------------------------------------------------------------
-- normalize Int to actual Int
-- ----------------------------------------------------------------------------

normalizeToInt :: Text -> Int
normalizeToInt = getInt

denormalizeFromInt :: Int -> Text
denormalizeFromInt = T.pack . show


-- ----------------------------------------------------------------------------
-- normalize Int as Text
-- ----------------------------------------------------------------------------

normalizeToText' :: Word -> Word
normalizeToText' i
  = T.concat [pfx, zeros, nr]
  where
  (pfx,nr) = if T.take 1 i == "-"
             then ("0", T.drop 1 i)
             else ("1", i)
  elems    = 20 - T.length nr
  zeros    = T.replicate elems "0"

normalizeToText :: Text -> Text
normalizeToText t
  = if isInt t
    then normalizeToText' t
    else error "normalizeToText: invalid input"

denormalizeFromText :: Text -> Text
denormalizeFromText i
  = sign raw
  where
  sign = if T.take 1 i == "1" then id else ('-' `T.cons`)
  raw  = T.dropWhile (== '0') $ T.drop 1 i


-- ----------------------------------------------------------------------------
-- Validate int
-- ----------------------------------------------------------------------------

-- | parses int value - but valiation needs to be
--   done before this proxy! otherwise normalization will
--   throw error.
getInt :: Text -> Int
getInt int = case parse integer "" $ T.unpack int of
  Right pint -> pint
  _          -> error "getInt: invalid input"

-- | validates int values with boundary check
isInt :: Text -> Bool
isInt int = case parse integer "" $ T.unpack int of
  Right pint -> int == (T.pack . show $ pint)
  _          -> False

integer :: Parser Int
integer = rd <$> (plus <|> minus <|> number)
  where
  rd     = read :: String -> Int
  plus   = char '+' *> number
  minus  = (:) <$> char '-' <*> number
  number = many1 digit
