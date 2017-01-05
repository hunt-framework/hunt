{-# LANGUAGE BangPatterns #-}
module Fox.Analyze (
    Analyzer
  , newAnalyzer
  , runAnalyzer

  , Tokenizer
  , runTokenizer
  , tokenizeAlpha
  , tokenizeDigits
  , tokenizeNonWhitespace

  , Filter
  , runFilter
  , filter
  , identityFilter
  , composeFilter
  , mapFilter
  , filterNonEmpty

  ) where

import           Fox.Types

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text.Internal
import           Data.Text.Unsafe
import           Prelude hiding (filter, map)

-- | Split a @FieldValue@ into @Token@s.
newtype Analyzer = Analyzer { runAnalyzer :: FieldName -> FieldValue -> [Token] }

-- | Split @Token@s from @FieldValue@.
newtype Tokenizer = Tokenizer { runTokenizer :: FieldValue -> [Token] }

-- | Filters a sequence of @Token@s.
newtype Filter = Filter { runFilter :: [Token] -> [Token] }

instance Monoid Filter where
  mempty  = identityFilter
  mappend = composeFilter

-- | Create an @Analyzer@ from @Tokenizer@ and @Filter@.
newAnalyzer :: Tokenizer -> Filter -> Analyzer
newAnalyzer tokenizer filters = Analyzer $ \_ value ->
  runFilter filters (runTokenizer tokenizer value)

tokenizeAlpha :: Tokenizer
tokenizeAlpha = Tokenizer $ splitText (\c -> not (Char.isAlphaNum c))

tokenizeDigits :: Tokenizer
tokenizeDigits = Tokenizer $ splitText (\c -> not (Char.isDigit c))

tokenizeNonWhitespace :: Tokenizer
tokenizeNonWhitespace = Tokenizer $ splitText (\c -> Char.isSpace c)

filter :: (Token -> Bool) -> Filter
filter p = Filter $ \xs -> List.filter p xs

identityFilter :: Filter
identityFilter = Filter $ \xs -> xs

composeFilter :: Filter -> Filter -> Filter
composeFilter f g = Filter $ \xs ->
  runFilter g (runFilter f xs)

mapFilter :: (Token -> Token) -> Filter
mapFilter f = Filter $ \xs -> List.map f xs

filterNonEmpty :: Filter
filterNonEmpty = filter (not . nullToken)

splitText :: (Char -> Bool) -> FieldValue -> [Token]
splitText p v =
  case v of
    FV_Text s -> go s
    _         -> go Text.empty
  where go s = split p s
{-# INLINE splitText #-}

split :: (Char -> Bool) -> Text -> [Token]
split isDelim t@(Text arr off len) = loop 0 0 0
  where
    loop !i !start !n
      | n >= len = if start == n
                   then []
                   else [Token i (Text arr (start+off) (n-start))]
      | isDelim c =
          if start == n
          then loop i (start+1) (start+1)
          else (Token i (Text arr (start+off) (n-start))) : loop (i+1) (n+d) (n+d)
      | otherwise = loop i start (n+d)
      where !(Iter !c !d) = iter t n
{-# INLINE split #-}
