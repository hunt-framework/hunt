{-# LANGUAGE BangPatterns #-}
module Hunt.Index.Schema.Tokenize(
    idTokenizer
  , alphaTokenizer
  , digitTokenizer
  , regexTokenizer
  , separatorTokenizer
  , spaceTokenizer
  ) where

import           Hunt.Common.BasicTypes

import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text.Internal
import           Data.Text.Unsafe

regexTokenizer :: RegEx -> Text -> [Text]
regexTokenizer = regExTokenize

-- | Doesn't tokenize at all! Mostly used for optimsation of some regexp.
idTokenizer :: Text -> [Text]
idTokenizer = pure

separatorTokenizer :: Text -> Text -> [Text]
separatorTokenizer
  = Text.splitOn

alphaTokenizer :: Text -> [Text]
alphaTokenizer
  = split (not . Char.isAlphaNum)

digitTokenizer :: Text -> [Text]
digitTokenizer
  = split (not . Char.isDigit)

spaceTokenizer :: Text -> [Text]
spaceTokenizer
  = split Char.isSpace

split :: (Char -> Bool) -> Text -> [Text]
split isDelim t@(Text arr off len) = loop 0 0
  where
    loop !start !n
        | n >= len = if start == n
                     then []
                     else [Text arr (start+off) (n-start)]
        | isDelim c =
            if start == n
            then loop (start+1) (start+1)
            else Text arr (start+off) (n-start) : loop (n+d) (n+d)
        | otherwise = loop start (n+d)
        where Iter c d = iter t n
{-# INLINE split #-}
