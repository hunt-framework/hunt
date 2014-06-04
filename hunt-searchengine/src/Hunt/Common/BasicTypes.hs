{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  Common types used within Hunt.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.BasicTypes
where

import           Control.Applicative
import           Control.Monad       (mzero)

import           Data.Map
import           Data.Text

import           Data.Aeson
import           Data.Binary         hiding (Word)

import           Hunt.Common.DocDesc (DocDesc)

-- ------------------------------------------------------------

-- | The URI describing the location of the original document.
type URI          = Text

-- | The description of a document is a generic key value map.
type Description  = DocDesc Text

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

-- | Weight (for ranking).
type Weight       = Float

-- | Regular expression.
type RegEx        = Text

-- | The score of a hit (either a document hit or a word hit).
type Score        = Float

-- ------------------------------------------------------------

-- | Weight of API documents,
-- @0.0@ indicates: not set, so there is no need to work with for Maybe's
--  wrapped in newtype to not mix up with Score's and Weight's in documents

newtype ApiWeight = AW Float
    deriving (Eq, Show)

noWeight :: ApiWeight
noWeight = AW 0.0

mkWeight :: Float -> ApiWeight
mkWeight x
    | x > 0.0   = AW x
    | otherwise = AW 0.0

getWeight :: ApiWeight -> Maybe Float
getWeight (AW 0.0) = Nothing
getWeight (AW x  ) = Just x

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

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

-- ------------------------------------------------------------
-- Binary instances
-- ------------------------------------------------------------

instance Binary ApiWeight where
    put (AW x) = put x
    get = AW <$> get

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
