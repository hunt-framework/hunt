module Holumbus.Index.Schema.Normalize.Int
where

import           Data.Maybe          (isJust, fromMaybe)
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Text.Read

import           Holumbus.Common.BasicTypes

-- ----------------------------------------------------------------------------
-- normalize Int to actual Int
-- ----------------------------------------------------------------------------

normalizeToInt :: Text -> Int
normalizeToInt t
  = case getInt t of
      (Right int) -> int
      _           -> error "if this fails, a validation is missing"

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

-- | this is basic validation - but valiation needs to be
--   done before this proxy! otherwise normalization will
--   throw error.
getInt :: Text -> Either () Int
getInt v
  = if isJust mv && v == smv
    then Right pv
    else Left ()
  where
  -- check for parseable int
  mv  = readMaybe (T.unpack v) :: Maybe Int
  pv  = fromMaybe 0 mv
  -- check for overflows
  smv = T.pack . show $ pv

-- | Check if value is a valid position
-- a valid position has a format like "double double"
-- a space is seperating the long/lat values
isInt :: Text -> Bool
isInt v = case getInt v of
            (Right _) -> True
            _         -> False
