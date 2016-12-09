{-# LANGUAGE BangPatterns #-}
module Fox.Analyze (
    Token
  , Analyzer
  , runAnalyzer
  , Tokenizer
  , runTokenizer
  , Filter
  , runFilter
  , newAnalyzer
  , tokenizeAlpha
  , tokenizeDigits
  , tokenizeNonWhitespace
  ) where

import           Fox.Types

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text.Internal
import           Data.Text.Unsafe

type Token = Text

-- | Split a @FieldValue@ into @Token@s.
newtype Analyzer = Analyzer { runAnalyzer :: FieldValue -> [Token] }

-- | Split @Token@s from @FieldValue@.
newtype Tokenizer = Tokenizer { runTokenizer :: FieldValue -> [Token] }

-- | Filters a sequence of @Token@s.
newtype Filter = Filter { runFilter :: [Token] -> [Token] }

-- | Create an @Analyzer@ from @Tokenizer@ and @Filter@.
newAnalyzer :: Tokenizer -> Filter -> Analyzer
newAnalyzer tokenizer filters = Analyzer $ \value ->
  runFilter filters (runTokenizer tokenizer value)

tokenizeAlpha :: Tokenizer
tokenizeAlpha = Tokenizer $ splitText (not . Char.isAlphaNum)

tokenizeDigits :: Tokenizer
tokenizeDigits = Tokenizer $ splitText (not . Char.isDigit)

tokenizeNonWhitespace :: Tokenizer
tokenizeNonWhitespace = Tokenizer $ splitText Char.isSpace

filter :: (Token -> Bool) -> Filter
filter p = Filter $ \xs -> List.filter p xs

splitText :: (Char -> Bool) -> FieldValue -> [Token]
splitText p (FV_Text s) = split p s
splitText p _           = []
{-# INLINE splitText #-}

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
