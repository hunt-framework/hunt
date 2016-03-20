{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- ----------------------------------------------------------------------------

{- |
  Common types used within Hunt.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.BasicTypes
where

import           Control.Monad

import           Data.Aeson
import           Data.Binary         hiding (Word)
import           Data.Map
import           Data.String
import           Data.Text

import           Hunt.Common.DocDesc (DocDesc)

import qualified Text.Regex.XMLSchema.Generic as HXT

import           Prelude             as P hiding (Word)

-- ------------------------------------------------------------

-- | The URI describing the location of the original document.
type URI          = Text

-- | The description of a document is a generic key value map.
type Description  = DocDesc

-- | The title of a document.
type Title        = Text

-- | The content of a document.
type Content      = Text

-- | The position of a word in the document.
type Position     = Int

-- | The name of a context.
type Context      = Text

-- | A single word.
type Word         = Text

-- | Positions of Words for each context.
type Words        = Map Context WordList

-- | Positions of words in the document.
type WordList     = Map Word [Position]

-- | Text index
data TextSearchOp = Case | NoCase | PrefixCase | PrefixNoCase
  deriving (Eq, Show)

-- | Regular expression. We remember the original expression
-- as HXT may blow up the representation.
data RegExp = RegExp { reTokenize  :: !(Text -> [Text])
                     , rePrintable :: !Text
                     }

mkRegexp :: Text -> Maybe RegExp
mkRegexp t | HXT.isZero re = Nothing
           | otherwise = Just RegExp { reTokenize = HXT.tokenizeRE re
                                     , rePrintable = t
                                     }
  where
    re = HXT.parseRegex t

-- ------------------------------------------------------------

instance Eq RegExp where
  re1 == re2 =
    rePrintable re1 == rePrintable re2

instance Show RegExp where
  show (RegExp _ s) = unpack s

instance IsString RegExp where
  fromString s =
    case mkRegexp (pack s) of
      Just re -> re
      Nothing -> error "You shouldonly use IsString instances for static regexes"

instance FromJSON RegExp where
  parseJSON (String s) =
    case mkRegexp s of
      Just re -> return re
      Nothing -> mzero
  parseJSON _
    = mzero

instance ToJSON RegExp where
  toJSON = String . rePrintable

instance Binary RegExp where
  put
    = put . rePrintable
  get
    = do s <- get
         case mkRegexp s of
           Just re -> return re
           _ -> mzero

instance FromJSON TextSearchOp where
    parseJSON (String s)
        = case s of
            "case"         -> return Case
            "noCase"       -> return NoCase
            "prefixCase"   -> return PrefixCase
            "prefixNoCase" -> return PrefixNoCase
            _              -> mzero
    parseJSON _ = mzero

instance ToJSON TextSearchOp where
  toJSON o = case o of
    Case         -> "case"
    NoCase       -> "noCase"
    PrefixCase   -> "prefixCase"
    PrefixNoCase -> "prefixNoCase"

instance Binary TextSearchOp where
  put (Case)         = put (0 :: Word8)
  put (NoCase)       = put (1 :: Word8)
  put (PrefixCase)   = put (2 :: Word8)
  put (PrefixNoCase) = put (3 :: Word8)

  get = do
    t <- get :: Get Word8
    case t of
      0 -> return Case
      1 -> return NoCase
      2 -> return PrefixCase
      3 -> return PrefixNoCase
      _ -> fail "enum out of bounds: TextSearchOp"

-- ------------------------------------------------------------
