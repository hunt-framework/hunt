module Holumbus.Index.Schema.Normalize.Int
where

import           Data.Maybe          (isJust, fromMaybe)
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Text.Read

import           Holumbus.Common.BasicTypes
import           Text.ParserCombinators.Parsec
import           Text.Parsec
import           Control.Applicative hiding ((<|>))

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
  elems    = 20 - (T.length nr)
  zeros    = T.pack $ foldr (\_ xs -> '0':xs) "" [1..elems]


normalizeToText :: Text -> Text
normalizeToText t
  = if isInt t
    then normalizeToText' t
    else error "if this fails, a validation is missing"

denormalizeFromText :: Text -> Text
denormalizeFromText i
  = T.concat [ sign, raw ]
  where
  sign = if T.take 1 i == "1" then "" else "-"
  raw  = T.dropWhile (== '0') $ T.drop 1 i


-- ----------------------------------------------------------------------------
-- Validate int
-- ----------------------------------------------------------------------------

-- | parses int value - but valiation needs to be
--   done before this proxy! otherwise normalization will
--   throw error.
getInt :: Text -> Int
getInt int = case parse integer "" $ T.unpack int of
      (Right int) -> int
      _           -> error "integer normalizor: not a valid integer"

-- | validates int values with boundary check
isInt :: Text -> Bool
isInt int = case parse integer "" $ T.unpack int of
      (Right pint) -> int == (T.pack . show $ pint)
      _            -> False

integer :: Parser Int
integer = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Int
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit
