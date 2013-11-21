module Holumbus.Common.Schema where

import Control.Monad                   (mzero)

import Data.Aeson
import Data.Text
import Data.Map
import Data.Binary

import Holumbus.Common.BasicTypes

-- ----------------------------------------------------------------------------

-- | Schema
type ContextSchema
  = Map Context ContextType

-- | The type can be any of the supported basic index types.
--   The regexp validates and splits the text into words:
--     []  -> invalid
--     xs  -> words/tokens
--   Every type can have a type-specific regexp.
--     => This means is has to match both the type-specific and the context-specific regexp.
--        Example: A CDate text has to match the type-specific regexp (XMLSchema-Date)
--                 (requirement for the corresponding Date-Parser which is used to normalize)
--   Every type can have a type-specific normalizer.
--     => This means it is first transformed by the type-specific normalizer and then by the
--        context-specific normalizers
--
--   /TL;DR/
--   Every input for both search and insert has
--     - two regexps    for validation and tokenization
--     - two normalizer for transformation
--   The first  regexp/normalizer is type-specific and is applied first (forced)
--   The second regexp/normalizer is context-specific (defined/chosen by user)
type ContextType
  = (CType, CRegex, [CNormalizer], CWeight)

-- | Types for values in a context.
data CType
  = CText
  | CInt
  deriving (Show, Eq)

-- | Regular expression.
type CRegex  = Text

-- | Enum for text-normalizers than can be chose by the user.
data CNormalizer = CUpperCase | CLowerCase
  deriving (Show, Eq)

-- | Context weight for search result rankings.
type CWeight = Float

-- ----------------------------------------------------------------------------
-- JSON instances
-- ----------------------------------------------------------------------------

instance FromJSON CType where
  parseJSON (String s)
    = case s of
        "ctext" -> return CText
        "cint"  -> return CInt
        _       -> mzero
  parseJSON _ = mzero

instance ToJSON CType where
  toJSON o = case o of
    CText -> "ctext"
    CInt  -> "cint"

instance FromJSON CNormalizer where
  parseJSON (String s)
    = case s of
        "uppercase" -> return CUpperCase
        "lowercase" -> return CLowerCase
        _           -> mzero
  parseJSON _ = mzero

instance ToJSON CNormalizer where
  toJSON o = case o of
    CUpperCase -> "uppercase"
    CLowerCase -> "lowercase"

-- ----------------------------------------------------------------------------
-- Binary instances
-- ----------------------------------------------------------------------------

instance Binary CType where
  put (CText) = put (0 :: Word8)
  put (CInt)  = put (1 :: Word8)

  get = do
    t <- get :: Get Word8
    case t of
      0 -> return CText
      1 -> return CInt

instance Binary CNormalizer where
  put (CUpperCase) = put (0 :: Word8)
  put (CLowerCase) = put (1 :: Word8)

  get = do
    t <- get :: Get Word8
    case t of
      0 -> return CUpperCase
      1 -> return CLowerCase
