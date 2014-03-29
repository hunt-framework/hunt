{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- ----------------------------------------------------------------------------
{- |
  Schema for the 'ContextIndex'.

  Every context has a type (e.g. text, int, date, position) and additional schema information.
  This includes how keys are splitted and normalized when inserted and searched for.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.Schema where

import           Control.Monad                        (mzero, liftM5)

import           Data.Aeson
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
import qualified Hunt.Index                           as Ix
import           Hunt.Index.InvertedIndex

import qualified Hunt.Index.Schema.Normalize.Position as Pos
import qualified Hunt.Index.Schema.Normalize.Int      as Int
import qualified Hunt.Index.Schema.Normalize.Date     as Date
import           Hunt.Utility

-- ------------------------------------------------------------

-- | The global schema assigning schema information to each context.
type Schema
  = Map Context ContextSchema

-- | The context schema information. Every context schema has a type and additional to adjust the
--   behavior.
--
--   The regular expression splits the text into words which are then transformed by the given
--   normalizations functions (e.g. to lower case).
data ContextSchema = ContextSchema
  {
    -- | Optional regex to override the default given by context type.
    cxRegEx      :: Maybe CRegex
    -- | Normalizers to apply on keys.
  , cxNormalizer :: [CNormalizer]
    -- | Context weight to boost results.
  , cxWeight     :: CWeight
    -- | If the context is searched in queries without context-specifier.
  , cxDefault    :: Bool
    -- | The type of the index (e.g. text, int, date, geo-position).
  , cxType       :: ContextType
  }
  deriving Show

-- | Context weight for search result rankings.
type CWeight = Float

-- | Regular expression.
type CRegex  = Text

-- ------------------------------------------------------------

instance Default ContextSchema where
  def = ContextSchema Nothing [] 1.0 True def

-- ------------------------------------------------------------

-- | Set of context types.
type ContextTypes = [ContextType]

-- | A general context type like text or int.
data ContextType = CType
  {
    -- | Name used in the (JSON) API.
    ctName     :: Text
    -- | Default regex to split words.
  , ctRegEx    :: CRegex
    -- | Validation function for keys.
  , ctValidate :: CValidator
    -- | The index implementation used for this type.
  , ctIxImpl   :: IndexImpl Occurrences
  }
  deriving Show

-- ------------------------------------------------------------

instance Default ContextType where
  def = ctText

-- ------------------------------------------------------------

-- | Text context type.
ctText :: ContextType
ctText = CType
  { ctName     = "text"
  , ctRegEx    = "\\w*"
  , ctValidate = def
  , ctIxImpl   = def
  }

-- | Int context type.
ctInt :: ContextType
ctInt = CType
  { ctName     = "int"
  , ctRegEx    = "([-]?[0-9]*)"
  , ctValidate = CValidator $ Int.isInt
  , ctIxImpl   = intInv
  }

-- | Date context type.
ctDate :: ContextType
ctDate = CType
  { ctName     = "date"
  , ctRegEx    = "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))"
  , ctValidate = CValidator $ Date.isAnyDate . unpack
  , ctIxImpl   = dateInv
  }

-- | Geographic position context type.
ctPosition :: ContextType
ctPosition = CType
  { ctName     = "position"
  , ctRegEx    = "-?(90(\\.0*)?|[1-8]?[0-9](\\.[0-9]*)?)--?((180(\\.0*)?)|(1[0-7][0-9])|([1-9]?[0-9]))(\\.[0-9]*)?"
  , ctValidate = CValidator $ Pos.isPosition
  , ctIxImpl   = positionInv
  }

-- ------------------------------------------------------------
-- IndexImpls
-- ------------------------------------------------------------

instance Default (IndexImpl Occurrences) where
  def = defaultInv

-- ------------------------------------------------------------

-- | Default (text) index implementation.
defaultInv :: IndexImpl Occurrences
defaultInv = mkIndex (Ix.empty :: InvertedIndex Occurrences)

-- | Int index implementation.
intInv :: IndexImpl Occurrences
intInv = mkIndex (Ix.empty :: InvertedIndexInt Occurrences)

-- | Date index implementation.
dateInv :: IndexImpl Occurrences
dateInv = mkIndex (Ix.empty :: InvertedIndexDate Occurrences)

-- | Geographic position index implementation.
positionInv :: IndexImpl Occurrences
positionInv = mkIndex (Ix.empty :: InvertedIndexPosition Occurrences)

-- ------------------------------------------------------------
-- Validator
-- ------------------------------------------------------------

-- | Validation function for single words.
data CValidator = CValidator { validate :: Word -> Bool }

-- ------------------------------------------------------------

instance Default CValidator where
  def = CValidator $ const True

-- XXX: maybe add name to validator type as well
instance Show CValidator where
  show _ = "CValidator"

-- ------------------------------------------------------------
-- Normalizer
-- ------------------------------------------------------------

data CNormalizer = CNormalizer
  { cnName       :: Text
  , cnNormalizer :: Text -> Text
  }

-- ------------------------------------------------------------

instance Show CNormalizer where
  show = unpack . cnName

instance Default CNormalizer where
  def = CNormalizer "" id

-- | Uppercase normalizer.
cnUpperCase :: CNormalizer
cnUpperCase = CNormalizer "UpperCase" T.toUpper

-- | Lowercase normalizer.
cnLowerCase :: CNormalizer
cnLowerCase = CNormalizer "LowerCase" T.toLower

-- | Int normalizer to preserve int ordering on strings.
cnZeroFill :: CNormalizer
cnZeroFill = CNormalizer "ZeroFill" Int.normalizeToText

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

-- | /Note/: This is only partional (de-)serialization.
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

-- ------------------------------------------------------------
-- Binary instances
-- ------------------------------------------------------------

instance Binary ContextSchema where
  get = liftM5 ContextSchema get get get get get
  put (ContextSchema a b c d e) = put a >> put b >> put c >> put d >> put e

instance Binary ContextType where
  put (CType n _ _ _) = put n
  get = get >>= \n -> return $ def { ctName = n }

instance Binary CNormalizer where
  put (CNormalizer n _) = put n
  get = get >>= \n -> return $ def { cnName = n }
