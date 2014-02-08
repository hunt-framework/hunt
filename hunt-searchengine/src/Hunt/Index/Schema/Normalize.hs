module Hunt.Index.Schema.Normalize
  ( contextNormalizer
--  , typeValidator
--  , rangeValidator
  )
where

import qualified Data.Text                            as T

import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema

import           Hunt.Index.Schema.Normalize.Position as Pos
import           Hunt.Index.Schema.Normalize.Date     as Date
import qualified Hunt.Index.Schema.Normalize.Int      as Int

-- ----------------------------------------------------------------------------

contextNormalizer :: CNormalizer -> Word -> Word
contextNormalizer o = case o of
  NormUpperCase   -> T.toUpper
  NormLowerCase   -> T.toLower
  NormDate        -> Date.normalize
  NormPosition    -> Pos.normalize
  NormIntZeroFill -> Int.normalizeToText

-- ----------------------------------------------------------------------------

-- | Checks if a range is valid for a context type.
{--rangeValidator :: CType -> [Text] -> [Text] -> Bool
rangeValidator t from to = case t of
   -- XXX TODO real range check for positions
   CPosition -> True
   _         -> defaultCheck from to
  where
  defaultCheck xs ys = fromMaybe False $ do
    x <- unboxM xs
    y <- unboxM ys
    return $ x <= y
--}
-- ----------------------------------------------------------------------------
