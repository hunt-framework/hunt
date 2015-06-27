module Hunt.Index.Schema.Tokenize where

import           Hunt.Common.BasicTypes

import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text

regexTokenizer :: RegEx -> Text -> [Text]
regexTokenizer = regExTokenize

separatorTokenizer :: Text -> Text -> [Text]
separatorTokenizer
  = Text.splitOn

alphaTokenizer :: Text -> [Text]
alphaTokenizer
  = List.filter (not . Text.null) . Text.split (not . Char.isAlpha)

digitTokenizer :: Text -> [Text]
digitTokenizer
  = List.filter (not . Text.null) . Text.split (not . Char.isDigit)

spaceTokenizer :: Text -> [Text]
spaceTokenizer
  = Text.words
