{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  Schema for the 'ContextIndex'.

  Every context has a type (e.g. text, int, date, position) and additional schema information.
  This includes how keys are splitted and normalized when inserted and searched for.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.Schema
  (
    -- * Types
    Schema
  , ContextSchema (..)
  , ContextType (..)
  , ContextTypes
  , CValidator (..)
  , CNormalizer (..)
  , TokenizerType(..)
  , CTokenizer(..)
  , mkDefaultTokenizer

  , normalize'

    -- * Default Context Types
  , ctText
  , ctTextSimple
  , ctInt
  , ctDate
  , ctPosition
  , ctPositionRTree

  -- * Default Normalizers
  , cnUpperCase
  , cnLowerCase
  , cnZeroFill
  )
where

import           Control.Applicative
import           Control.Monad (mzero)

import           Data.Aeson
import           Data.Binary hiding (Word)
import           Data.Default
import qualified Data.List as L
import           Data.Map hiding (null)
import           Data.Maybe (isNothing)
import           Data.Text hiding (null)
import qualified Data.Text as T
import           Data.Text.Binary ()

import           Hunt.Common.BasicTypes
import qualified Hunt.Index as Ix
import           Hunt.Index.IndexImpl (IndexImpl, mkIndex)
import           Hunt.Index.InvertedIndex
import           Hunt.Index.Merge
import           Hunt.Index.PrefixTreeIndex           ( PrefixTreeIndexInt
                                                      , PrefixTreeIndexDate
                                                      , SimplePrefixTreeIndex )
import           Hunt.Index.PrefixTreeIndex2Dim ( PrefixTreeIndexPosition )
import           Hunt.Index.RTreeIndex
import qualified Hunt.Index.Schema.Normalize.Date as Date
import qualified Hunt.Index.Schema.Normalize.Int as Int
import qualified Hunt.Index.Schema.Normalize.Position as Pos
import           Hunt.Scoring.Score (Score)
import           Hunt.Utility
import qualified Hunt.Index.Schema.Tokenize as Tokenize

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
    -- | Optional tokenizer to override the default given by context type.
    cxTokenizer  :: Maybe CTokenizer
    -- | Normalizers to apply on keys.
  , cxNormalizer :: [CNormalizer]
    -- | Context weight to boost results.
  , cxWeight     :: Score
    -- | Whether the context is searched in queries without context-specifier.
  , cxDefault    :: Bool
    -- | The type of the index (e.g. text, int, date, geo-position).
  , cxType       :: ContextType
  }
  deriving Show

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
    -- | Default tokenizer for words
  , ctTokenizer :: CTokenizer
    -- | Validation function for keys.
  , ctValidate :: CValidator
    -- | The index implementation used for this type.
  , ctIxImpl   :: IndexImpl
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
  , ctTokenizer = mkDefaultTokenizer TokenizeAlpha
  , ctValidate = def
  , ctIxImpl   = def
  }

-- | Special text context type
--   smaller index but not phrase queries possible
--   (due to not storing the words positions)
ctTextSimple :: ContextType
ctTextSimple = CType
  { ctName     = "text-small"
  , ctTokenizer = mkDefaultTokenizer TokenizeAlpha
  , ctValidate = def
  , ctIxImpl   = simplePT
  }

-- | Int context type.
ctInt :: ContextType
ctInt = CType
  { ctName     = "int"
  , ctTokenizer = mkDefaultTokenizer TokenizeDigit
  , ctValidate = CValidator $ Int.isInt
  , ctIxImpl   = intInv
  }

-- | Date context type.
ctDate :: ContextType
ctDate = CType
  { ctName     = "date"
  , ctTokenizer = mkDefaultTokenizer (TokenizeRegEx "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))")
  , ctValidate = CValidator $ Date.isAnyDate . unpack
  , ctIxImpl   = dateInv
  }

-- | Geographic position context type.
ctPosition :: ContextType
ctPosition = CType
  { ctName     = "position"
  , ctTokenizer = mkDefaultTokenizer (TokenizeRegEx "-?(90(\\.0*)?|[1-8]?[0-9](\\.[0-9]*)?)--?((180(\\.0*)?)|(1[0-7][0-9])|([1-9]?[0-9]))(\\.[0-9]*)?")
  , ctValidate = CValidator $ Pos.isPosition
  , ctIxImpl   = positionInv
  }

ctPositionRTree :: ContextType
ctPositionRTree = CType
  { ctName     = "position-rtree"
  , ctTokenizer = mkDefaultTokenizer (TokenizeRegEx "-?(90(\\.0*)?|[1-8]?[0-9](\\.[0-9]*)?)--?((180(\\.0*)?)|(1[0-7][0-9])|([1-9]?[0-9]))(\\.[0-9]*)?")
  , ctValidate = CValidator $ Pos.isPosition
  , ctIxImpl   = positionRTree
  }


-- ------------------------------------------------------------
-- IndexImpls
-- ------------------------------------------------------------

instance Default IndexImpl where
  def = defaultInv

-- ------------------------------------------------------------

-- | Default (text) index implementation.
defaultInv :: IndexImpl
defaultInv = mkIndex (Ix.empty :: InvertedIndex)

-- | Simpler (text) index, which still enables prefix search,
--   but not phrase search anymore. Useful for cases where
--   word positions are not relevant
simplePT :: IndexImpl
simplePT = mkIndex (Ix.empty :: SimplePrefixTreeIndex)

-- | Int index implementation.
intInv :: IndexImpl
intInv = mkIndex (Ix.empty :: PrefixTreeIndexInt)

-- | Date index implementation.
dateInv :: IndexImpl
dateInv = mkIndex (Ix.empty :: PrefixTreeIndexDate)

-- | Geographic position index implementation based on 'StringMap'
positionInv :: IndexImpl
positionInv = mkIndex (Ix.empty :: PrefixTreeIndexPosition)

-- | Geographic position index implementation based on 'RTree'
positionRTree :: IndexImpl
positionRTree = mkIndex (Ix.empty :: SimpleRTreeIndex)


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

-- | Normalizer for words\/keys of an index.
data CNormalizer = CNormalizer
  { cnName    :: Text         -- ^ Name used in (JSON) API.
  , normalize :: Text -> Text -- ^ Normalization function.
  }

-- | Apply the normalizers to a word.
normalize' :: [CNormalizer] -> Word -> Word
normalize' ns  = L.foldl' (\f2 (CNormalizer _ f1) -> f1 . f2) id $ ns

-- ------------------------------------------------------------

instance Show CNormalizer where
  show = unpack . cnName

instance Default CNormalizer where
  def = CNormalizer "" id

-- | Uppercase normalizer \"UpperCase\".
cnUpperCase :: CNormalizer
cnUpperCase = CNormalizer "UpperCase" T.toUpper

-- | Lowercase normalizer \"LowerCase\".
cnLowerCase :: CNormalizer
cnLowerCase = CNormalizer "LowerCase" T.toLower

-- | Int normalizer \"ZeroFill\" to preserve int ordering on strings.
cnZeroFill :: CNormalizer
cnZeroFill = CNormalizer "ZeroFill" Int.normalizeToText

-- ------------------------------------------------------------
-- Tokenizer
-- ------------------------------------------------------------

data TokenizerType
  = TokenizeRegEx RegEx
  | TokenizeSeparator Text
  | TokenizeAlpha
  | TokenizeDigit
  | TokenizeSpace
  | TokenizeCustom Text
  deriving (Eq)

data CTokenizer
  = CTokenizer { ctkType     :: TokenizerType
               , ctkTokenize :: Text -> [Text]
               }

mkDefaultTokenizer :: TokenizerType -> CTokenizer
mkDefaultTokenizer tt
  = case tt of
      TokenizeRegEx re      -> CTokenizer tt (Tokenize.regexTokenizer re)
      TokenizeSeparator sep -> CTokenizer tt (Tokenize.separatorTokenizer sep)
      TokenizeAlpha         -> CTokenizer tt Tokenize.alphaTokenizer
      TokenizeDigit         -> CTokenizer tt Tokenize.digitTokenizer
      TokenizeSpace         -> CTokenizer tt Tokenize.spaceTokenizer
      _                     -> error "custom tokenizer is no default"

instance Show CTokenizer where
  show (CTokenizer t _) = show t

instance Default CTokenizer where
  def = mkDefaultTokenizer TokenizeAlpha

instance FromJSON CTokenizer where
  parseJSON o
    = do tkType <- parseJSON o
         return def { ctkType = tkType
                    }

instance ToJSON CTokenizer where
  toJSON (CTokenizer ctkType _)
    = toJSON ctkType

instance Binary CTokenizer where
  get = undefined
  put = undefined

instance Show TokenizerType where
  show (TokenizeRegEx _)     = "Regex"
  show (TokenizeSeparator _) = "Separator"
  show TokenizeAlpha         = "Alpha"
  show TokenizeDigit         = "Digit"
  show TokenizeSpace         = "Whitespace"
  show (TokenizeCustom n)    = T.unpack n

instance FromJSON TokenizerType where
  parseJSON (Object o)
    = do tkName <- o .: "type"
         case tkName of
           "Regex"     -> TokenizeRegEx <$> o .: "regex"
           "Separator" -> TokenizeSeparator <$> o .: "separator"
           "Alpha"     -> pure TokenizeAlpha
           "Digit"     -> pure TokenizeDigit
           "Whitespace"-> pure TokenizeSpace
           x           -> pure (TokenizeCustom x)
           _           -> mzero
  parseJSON s
    = TokenizeRegEx <$> parseJSON s

instance ToJSON TokenizerType where
  toJSON (TokenizeRegEx re)
    = object [ "type" .= ("Regex" :: Text), "regex" .= re ]
  toJSON (TokenizeSeparator sep)
    = object [ "type" .= ("Separator" :: Text), "separator" .= sep ]
  toJSON TokenizeAlpha
    = object [ "type" .= ("Alpha" :: Text) ]
  toJSON TokenizeDigit
    = object [ "type" .= ("Digit" :: Text) ]
  toJSON TokenizeSpace
    = object [ "type" .= ("Whitespace" :: Text) ]
  toJSON (TokenizeCustom name)
    = object [ "type" .= name ]

instance Binary TokenizerType where
  put = undefined
  get = undefined

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

-- | /Note/: This is only partial (de-)serialization.
--   The other components are environment-dependent
--   and cannot be (de-)serialized. We serialize the name
--   and identify the other compontens of the type later.

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
    t  <- o .:? "tokenizer"
    r  <- o .:? "regex"
    n  <- o .:? "normalizers" .!= []
    w  <- o .:? "weight"      .!= 1.0
    d  <- o .:? "default"     .!= True
    ct <- o .:  "type"
    return $ ContextSchema (t <|> r) n w d ct

  parseJSON _ = mzero

instance ToJSON ContextSchema where
  toJSON (ContextSchema t n w d ct) = object' $
    [ "type"        .== ct
    , "weight"      .=? w .\. (== 1.0)
    , "tokenizer"   .=? t .\. isNothing
    , "normalizers" .=? n .\. null
    , "default"     .=? d .\. id
    ]

-- ------------------------------------------------------------
-- Binary instances
-- ------------------------------------------------------------

instance Binary ContextSchema where
  get = ContextSchema <$> get <*> get <*> get <*> get <*> get
  put (ContextSchema a b c d e) = put a >> put b >> put c >> put d >> put e

instance Binary ContextType where
  put (CType n _ _ _) = put n
  get = get >>= \n -> return $ def { ctName = n }

instance Binary CNormalizer where
  put (CNormalizer n _) = put n
  get = get >>= \n -> return $ def { cnName = n }
