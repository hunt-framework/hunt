{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hunt.Index.Schema where

import           Control.Monad                        (mzero, liftM5)

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Binary                          hiding (Word)
import           Data.Map                             hiding (null)
import           Data.Text                            hiding (null)
import qualified Data.Text                            as T
import           Data.Text.Binary                     ()
import           Data.Maybe                           (isNothing)
import           Data.Default

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences              (Occurrences)
import           Hunt.Index.IndexImpl                 (IndexImpl, mkIndex)
import qualified Hunt.Index.Index                     as Ix
import           Hunt.Index.InvertedIndex

import qualified Hunt.Index.Schema.Normalize.Position as Pos
import qualified Hunt.Index.Schema.Normalize.Int      as Int
import qualified Hunt.Index.Schema.Normalize.Date     as Date

-- ----------------------------------------------------------------------------

-- | Schema
type Schema
  = Map Context ContextSchema

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
data ContextSchema = ContextSchema
  {
  -- optional regex to overwrite default given by context type
    cxRegEx      :: Maybe CRegex
  -- normalizers to apply
  , cxNormalizer :: [CNormalizer]
  -- context weight
  , cxWeight     :: CWeight
  -- should this context used in non-context queries?
  , cxDefault    :: Bool
  -- contexttype
  , cxType       :: ContextType
  }
  deriving Show

-- | Context weight for search result rankings.
type CWeight = Float

-- | Regular expression.
type CRegex  = Text

-- ----------------------------------------------------------------------------

instance Default ContextSchema where
  def = ContextSchema Nothing [] 1.0 True def

-- ----------------------------------------------------------------------------

type ContextTypes = [ContextType]

data ContextType = CType
  -- name of the context type
  { ctName     :: Text
  -- default regex used when no user defined regex is given in ContextSchema
  , ctRegEx    :: CRegex
  -- validator function which checks values
  , ctValidate :: CValidator
  -- index implementation used for this context type
  , ctIxImpl   :: IndexImpl Occurrences
  }
  deriving Show

-- ----------------------------------------------------------------------------

instance Default ContextType where
  def = ctText

-- ----------------------------------------------------------------------------

ctText :: ContextType
ctText = CType
  { ctName     = "text"
  , ctRegEx    = "\\w*"
  , ctValidate = def
  , ctIxImpl   = def
  }

ctInt :: ContextType
ctInt = CType
  { ctName     = "int"
  , ctRegEx    = "([-]?[0-9]*)"
  , ctValidate = CValidator $ Int.isInt
  , ctIxImpl   = intInv
  }

ctDate :: ContextType
ctDate = CType
  { ctName     = "date"
  , ctRegEx    = "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))"
  , ctValidate = CValidator $ Date.isAnyDate . unpack
  , ctIxImpl   = dateInv
  }

ctPosition :: ContextType
ctPosition = CType
  { ctName     = "position"
  , ctRegEx    = "-?(90(\\.0*)?|[1-8]?[0-9](\\.[0-9]*)?)--?((180(\\.0*)?)|(1[0-7][0-9])|([1-9]?[0-9]))(\\.[0-9]*)?"
  , ctValidate = CValidator $ Pos.isPosition
  , ctIxImpl   = positionInv
  }

-- ----------------------------------------------------------------------------
-- IndexImpls

instance Default (IndexImpl Occurrences) where
  def = defaultInv


defaultInv :: IndexImpl Occurrences
defaultInv = mkIndex (Ix.empty :: InvertedIndex Occurrences)

intInv :: IndexImpl Occurrences
intInv = mkIndex (Ix.empty :: InvertedIndexInt Occurrences)

dateInv :: IndexImpl Occurrences
dateInv = mkIndex (Ix.empty :: InvertedIndexDate Occurrences)

positionInv :: IndexImpl Occurrences
positionInv = mkIndex (Ix.empty :: InvertedIndexPosition Occurrences)

-- ----------------------------------------------------------------------------
-- Validator

data CValidator = CValidator { validate :: Word -> Bool }

-- ----------------------------------------------------------------------------

instance Default CValidator where
  def = CValidator $ const True

-- | XXX maybe add name to validator type as well
instance Show CValidator where
  show _ = "CValidator"

-- ----------------------------------------------------------------------------
-- Normalizer

data CNormalizer = CNormalizer
  { cnName       :: Text
  , cnNormalizer :: Text -> Text
  }

-- ----------------------------------------------------------------------------

-- | Enum for text-normalizers than can be chose by the user.
--data CNormalizer = NormUpperCase | NormLowerCase | NormDate | NormPosition | NormIntZeroFill
--  deriving (Show, Eq)

instance Show CNormalizer where
  show = unpack . cnName

instance Default CNormalizer where
  def = CNormalizer "" id

cnUpperCase :: CNormalizer
cnUpperCase = CNormalizer "UpperCase" T.toUpper

cnLowerCase :: CNormalizer
cnLowerCase = CNormalizer "LowerCase" T.toLower

cnZeroFill :: CNormalizer
cnZeroFill = CNormalizer "ZeroFill" Int.normalizeToText

-- ----------------------------------------------------------------------------
-- JSON instances
-- ----------------------------------------------------------------------------

-- | Note: This is only partional (de-)serialization.
--   The other components are environment depending
--   and cannot be (de-)serialized. We serialize the name
--   and identify the other compontens of the type
--   later.
instance FromJSON ContextType where
  parseJSON (String s) = return $ def { ctName = s }
  parseJSON _          = mzero

instance ToJSON ContextType where
  toJSON (CType n _ _ _) = String n

instance FromJSON CNormalizer where
  parseJSON (String s) = return $ def { cnName = s }
  parseJSON _          = mzero

instance ToJSON CNormalizer where
  toJSON (CNormalizer n _) = String n

instance FromJSON ContextSchema where
  parseJSON (Object o) = do
    r <- o .:? "regexp"
    n <- o .:? "normalizers" .!= []
    w <- o .:? "weight"      .!= 1.0
    d <- o .:? "default"     .!= True
    ct <- o .: "type"
    return $ ContextSchema r n w d ct

  parseJSON _ = mzero

instance ToJSON ContextSchema where
  toJSON (ContextSchema r n w d ct) = object . Prelude.concat $
    [ [ "type"       .= ct
      , "weight"      .= w
      ]
    , "regexp"        .=? r .\. isNothing
    , "normalizers"   .=? n .\. null
    , "default"       .=? d .\. id
    ]

-- ----------------------------------------------------------------------------
-- Binary instances
-- ----------------------------------------------------------------------------

instance Binary ContextSchema where
  get = liftM5 ContextSchema get get get get get
  put (ContextSchema a b c d e) = put a >> put b >> put c >> put d >> put e

instance Binary ContextType where
  put (CType n _ _ _) = put n
  get = get >>= \n -> return $ def { ctName = n }

instance Binary CNormalizer where
  put (CNormalizer n _) = put n
  get = get >>= \n -> return $ def { cnName = n }

-- ----------------------------------------------------------------------------
-- Aeson helper
-- ----------------------------------------------------------------------------

(.=?) :: ToJSON a => Text -> (a, a -> Bool) -> [Pair]
name .=? (value, cond) = if cond value then [] else [ name .= value ]

(.\.) :: ToJSON a => a -> (a -> Bool) -> (a, a -> Bool)
v .\. c = (v,c)
infixl 8 .=?
