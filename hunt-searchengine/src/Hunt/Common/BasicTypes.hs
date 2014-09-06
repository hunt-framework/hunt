{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- ----------------------------------------------------------------------------

{- |
  Common types used within Hunt.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.BasicTypes
    ( module Hunt.Common.BasicTypes
    , Monoid(..)
    , (<>)
    )
where

import           Control.Applicative
import           Control.Monad       (mzero)
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary         hiding (Word)
import           Data.Map
import           Data.Monoid         (Monoid (..), (<>))
import           Data.Text

import           Hunt.Common.DocDesc (DocDesc)

import           Prelude             as P

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

-- | Weight (for ranking).
type Weight       = Score

-- | Regular expression.
type RegEx        = Text

-- | The score of a hit (either a document hit or a word hit).
-- type Score        = Float

-- ------------------------------------------------------------

-- | Weight or score of a documents,
-- @0.0@ indicates: not set, so there is no need to work with Maybe's
--  wrapped in newtype to not mix up with Score's and Weight's in documents

newtype Score = SC {unScore :: Float}
    deriving (Eq, Ord, Num, Fractional, Show)

instance NFData Score where
  rnf (SC f) = f `seq` ()

noScore :: Score
noScore = SC 0.0

mkScore :: Float -> Score
mkScore x
    | x > 0.0   = SC x
    | otherwise = noScore

getScore :: Score -> Maybe Float
getScore (SC 0.0) = Nothing
getScore (SC x  ) = Just x

defScore :: Score
defScore = SC 1.0

toDefScore :: Score -> Score
toDefScore (SC 0.0) = defScore
toDefScore sc       = sc

fromDefScore :: Score -> Score
fromDefScore (SC 1.0) = noScore
fromDefScore sc       = sc

accScore :: [Score] -> Score
accScore [] = defScore
accScore xs = mkScore $ sum (P.map unScore xs) / fromIntegral (P.length xs)

-- the Monoid instance is used to accumulate scores
-- in query results, so tune it here when sum is not appropriate

instance Monoid Score where
    mempty = noScore
    mappend = (+)

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

instance FromJSON Score where
    parseJSON x = mkScore <$> parseJSON x

instance ToJSON Score where
    toJSON (SC x) = toJSON x

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

instance Binary Score where
    put (SC x) = put x
    get = SC <$> get

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
