{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Index.Schema where

import           Control.Monad                           (mzero)

import           Data.Aeson
import           Data.Binary
import           Data.Map
import           Data.Text
import           Data.Text.Binary                         ()

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Occurrences              (Occurrences)
import           Holumbus.Index.IndexImpl                 (IndexImpl, mkIndex)
import qualified Holumbus.Index.Index                     as Ix
import           Holumbus.Index.InvertedIndex

import qualified Holumbus.Index.Schema.Normalize.Position as Pos
import qualified Holumbus.Index.Schema.Normalize.Int      as Int
import qualified Holumbus.Index.Schema.Normalize.Date     as Date

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
  -- name of the context
  { cxName       :: Text
  -- XXX: regex change to maybe - optional since we have a default within contexttype
  , cxRegEx      :: CRegex
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

ctEmpty :: ContextType
ctEmpty = CType
  { ctName     = ""
  , ctRegEx    = ""
  , ctValidate = defValid
  , ctIxImpl   = defaultInv
  }

defValid :: CValidator
defValid = CValidator $ \_ -> True

-- TODO: fix default validator and regex in all impls!
ctText :: ContextType
ctText = CType
  { ctName     = "text"
  , ctRegEx    = "\\w*"
  , ctValidate = defValid
  , ctIxImpl   = defaultInv
  }

ctInt :: ContextType
ctInt = CType
  { ctName     = "int"
  , ctRegEx    = "\\w*"
  , ctValidate = CValidator $ Int.isInt
  , ctIxImpl   = intInv
  }

ctDate :: ContextType
ctDate = CType
  { ctName     = "date"
  , ctRegEx    = "\\w*"
  , ctValidate = CValidator $ Date.isAnyDate . unpack
  , ctIxImpl   = dateInv
  }

ctPosition :: ContextType
ctPosition = CType
  { ctName     = "position"
  , ctRegEx    = "\\w*"
  , ctValidate = CValidator $ Pos.isPosition
  , ctIxImpl   = positionInv
  }

defaultInv :: IndexImpl Occurrences
defaultInv = mkIndex (Ix.empty :: InvertedIndex Occurrences)

intInv :: IndexImpl Occurrences
intInv = mkIndex (Ix.empty :: InvertedIndexInt Occurrences)

dateInv :: IndexImpl Occurrences
dateInv = mkIndex (Ix.empty :: InvertedIndexDate Occurrences)

positionInv :: IndexImpl Occurrences
positionInv = mkIndex (Ix.empty :: InvertedIndexPosition Occurrences)

data CValidator = CValidator { validate :: Text -> Bool }

instance Show CValidator where
  show _ = "CValidator"


-- | Regular expression.
type CRegex  = Text

-- | Enum for text-normalizers than can be chose by the user.
data CNormalizer = NormUpperCase | NormLowerCase | NormDate | NormPosition | NormIntZeroFill
  deriving (Show, Eq)

-- | Context weight for search result rankings.
type CWeight = Float

-- ----------------------------------------------------------------------------
-- JSON instances
-- ----------------------------------------------------------------------------

{--
instance FromJSON CType where
  parseJSON (String s)
    = case s of
        "text"     -> return CText
        "int"      -> return CInt
        "date"     -> return CDate
        "position" -> return CPosition
        _          -> mzero
  parseJSON _ = mzero

instance ToJSON CType where
  toJSON o = case o of
    CText     -> "text"
    CInt      -> "int"
    CDate     -> "date"
    CPosition -> "position"
--}
--

-- | Note that this is only parting serialization
instance FromJSON ContextType where
  parseJSON (String s) = return $ ctEmpty { ctName = s }
  parseJSON _          = mzero

instance ToJSON ContextType where
  toJSON (CType n _ _ _) = String n

instance FromJSON CNormalizer where
  parseJSON (String s)
    = case s of
        "uppercase" -> return NormUpperCase
        "lowercase" -> return NormLowerCase
        "date"      -> return NormDate
        "position"  -> return NormPosition
        "zerofill"  -> return NormIntZeroFill
        _           -> mzero
  parseJSON _ = mzero

instance ToJSON CNormalizer where
  toJSON o = case o of
    NormUpperCase   -> "uppercase"
    NormLowerCase   -> "lowercase"
    NormDate        -> "date"
    NormPosition    -> "position"
    NormIntZeroFill -> "zerofill"

instance FromJSON ContextSchema where
  parseJSON (Object o) = do
    t <- o .: "type"
    r <- o .: "regexp"
    n <- o .: "normalizers"
    w <- o .: "weight"
    d <- o .:? "default" .!= True
    ct <- o .: "type"
    return $ ContextSchema t r n w d ct

  parseJSON _ = mzero

instance ToJSON ContextSchema where
  toJSON (ContextSchema t r n w d ct) = object
    [ "type"        .= t
    , "regexp"      .= r
    , "normalizers" .= n
    , "weight"      .= w
    , "default"     .= d
    , "ctype"       .= ct
    ]

-- ----------------------------------------------------------------------------
-- Binary instances
-- ----------------------------------------------------------------------------

{--
instance Binary CType where
  put (CText)     = put (0 :: Word8)
  put (CInt)      = put (1 :: Word8)
  put (CDate)     = put (2 :: Word8)
  put (CPosition) = put (3 :: Word8)

  get = do
    t <- get :: Get Word8
    case t of
      0 -> return CText
      1 -> return CInt
      2 -> return CDate
      3 -> return CPosition
      _ -> fail "get(CType) out of bounds"
--}

instance Binary CNormalizer where
  put (NormUpperCase)   = put (0 :: Word8)
  put (NormLowerCase)   = put (1 :: Word8)
  put (NormDate)        = put (2 :: Word8)
  put (NormPosition)    = put (3 :: Word8)
  put (NormIntZeroFill) = put (4 :: Word8)

  get = do
    t <- get :: Get Word8
    case t of
      0 -> return NormUpperCase
      1 -> return NormLowerCase
      2 -> return NormDate
      3 -> return NormPosition
      4 -> return NormIntZeroFill
      _ -> fail "get(CNormalizer) out of bounds"

instance Binary ContextType where
  put (CType n _ _ _) = put n
  get = get >>= \n -> return $ ctEmpty { ctName = n }


instance Binary ContextSchema where
  get = liftM6 ContextSchema get get get get get get
  put (ContextSchema a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f

liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6
       ; return (f x1 x2 x3 x4 x5 x6)
       }
