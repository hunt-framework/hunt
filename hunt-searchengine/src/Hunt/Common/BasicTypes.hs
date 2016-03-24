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
import           Data.Binary hiding (Word)
import           Data.Map
import           Data.String
import           Data.Text
import           Hunt.Common.DocDesc (DocDesc)
import           Prelude hiding (Word)
import qualified Text.Regex.XMLSchema.Generic as HXT

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

-- | Regular expression.
data RegEx = RegEx { reCompiled  :: !HXT.RegexText
                   , rePrintable :: !Text
                   }
              deriving (Eq, Ord)

regExTokenize :: RegEx -> Text -> [Text]
regExTokenize (RegEx re _)
  = HXT.tokenizeRE re

-- ------------------------------------------------------------

instance Show RegEx where
  show (RegEx _ s) = unpack s

instance IsString RegEx where
  fromString s
    = RegEx { reCompiled  = re'
            , rePrintable = t
            }
    where
      t   = pack s
      re  = HXT.parseRegex t
      re' = if HXT.isZero re
            then error ("You should only use IsString instance for static regexes, error in regex: " ++ show s)
            else re

instance FromJSON RegEx where
  parseJSON (String s)
    = do when (HXT.isZero re) mzero
         return (RegEx re s)
    where
      re = HXT.parseRegex s
  parseJSON _
    = mzero

instance ToJSON RegEx where
  toJSON
    = String . rePrintable

instance Binary RegEx where
  put
    = put . rePrintable
  get
    = do s <- get
         let re = HXT.parseRegex s
         when (HXT.isZero re) mzero
         return (RegEx re s)

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
