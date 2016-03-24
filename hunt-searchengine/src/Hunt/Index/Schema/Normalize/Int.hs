{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  Normalization and validation for integer.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.Schema.Normalize.Int
  ( normalizeToText
  , denormalizeFromText
  , normalizeToInt
  , normalizeToInt'
  , denormalizeFromInt
  , isInt
  )
where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Hunt.Common.BasicTypes (Word)
import           Prelude hiding (Word)

-- ------------------------------------------------------------
-- Normalize Int to actual Int
-- ------------------------------------------------------------

-- | Normalize a text representing of an integer to an 'Int'.
normalizeToInt :: Text -> Int
normalizeToInt = maybe (error "normalizeToInt: invalid input") id
                 . normalizeToInt'

normalizeToInt' :: Text -> Maybe Int
normalizeToInt' t = do
  (n, "") <- either (const Nothing) Just (T.signed T.decimal t) :: Maybe (Integer, Text)
  if (n > fromIntegral (maxBound :: Int))
     || (n < fromIntegral (minBound :: Int))
    then Nothing
    else return (fromIntegral n)

isInt :: Text -> Bool
isInt = maybe False (const True)
        . normalizeToInt'

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
