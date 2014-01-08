module Holumbus.Index.Schema.Normalize
  ( contextNormalizer
  , typeValidator
  , rangeValidator
  )
where

import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text                                as T

import           Holumbus.Common.BasicTypes
import           Holumbus.Index.Schema
import           Holumbus.Utility

import           Holumbus.Index.Schema.Normalize.Position as Pos
import           Holumbus.Index.Schema.Normalize.Date     as Date
import qualified Holumbus.Index.Schema.Normalize.Int      as Int

-- ----------------------------------------------------------------------------

contextNormalizer :: CNormalizer -> Word -> Word
contextNormalizer o = case o of
    NormUpperCase   -> T.toUpper
    NormLowerCase   -> T.toLower
    NormDate        -> Date.normalize
    NormPosition    -> Pos.normalize
    NormIntZeroFill -> Int.normalizeToText

-- ----------------------------------------------------------------------------

-- | Checks if value is valid for a context type.
typeValidator :: CType -> Text -> Bool
typeValidator t = case t of
    CText     -> const True
    CInt      -> Int.isInt
    CPosition -> Pos.isPosition
    CDate     -> Date.isAnyDate . T.unpack

-- ----------------------------------------------------------------------------

-- | Checks if a range is valid for a context type.
rangeValidator :: CType -> [Text] -> [Text] -> Bool
rangeValidator t from to = case t of
   -- XXX TODO real range check for positions
   CPosition -> True
   _         -> defaultCheck from to
  where
  defaultCheck xs ys = fromMaybe False $ do
    x <- unboxM xs
    y <- unboxM ys
    return $ x <= y

-- ----------------------------------------------------------------------------
