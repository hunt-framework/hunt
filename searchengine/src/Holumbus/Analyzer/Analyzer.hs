module Holumbus.Analyzer.Analyzer
  ( toDocAndWords
  )
where

import           Data.DList                  (DList)
import qualified Data.DList                  as DL
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Text.Regex.XMLSchema.String

import           Holumbus.Common.Document    (Document (..))
import           Holumbus.Common.BasicTypes

import           Holumbus.Common.ApiDocument
import           Holumbus.Common.Schema

import           Holumbus.Analyzer.Normalizer

{--
 - since we have a very flexible index typeclass i think it would make sense
 - to have a typeclass for the analyzer as well that supports different output
 - types.
 -}

-- ----------------------------------------------------------------------------

{-
analyzerMapping :: AnalyzerType -> Text -> [(Position, Word)]
analyzerMapping o = case o of
    DefaultAnalyzer -> scanTextDefault
-}

-- | ApiDocument to Document and Words mapping.
toDocAndWords :: ContextSchema -> ApiDocument -> (Document, Words)
toDocAndWords schema apiDoc = (doc, ws)
  where
  indexMap = apiDocIndexMap apiDoc
  descrMap = apiDocDescrMap apiDoc
  doc = Document
          { uri   = apiDocUri apiDoc
          , desc  = descrMap
          }
  ws = M.mapWithKey (\context -> either
                id
                (\(TextData content metadata)
                    -- FIXME: make sure that all contexts exist, otherwise this will crash
                    -- TODO: discards index metadata in apidoc - obsolete now?
                    -> let (cType, rex, normType, w) = fromJust $ M.lookup context schema
                           scan = scanTextRE rex
                           norm = chainFuns . map normalizerMapping $ normType
                       in toWordList scan norm content)) indexMap

-- | Chain a list of functions.
chainFuns :: [a -> a] -> a -> a
chainFuns = foldl (.) id

-- | Construct a WordList from Text using the function scan to split
--   the text into words with their corresponding positions.
toWordList :: (Text -> [Word]) -> (Word -> Word) -> Text -> WordList
toWordList scan norm = M.map DL.toList . foldr insert M.empty . zip [1..] . map norm . scan
  where
  insert :: (Position, Word) -> Map Word (DList Position) -> Map Word (DList Position)
  insert (p, w)
    = M.alter (return . maybe (DL.singleton p) (`DL.snoc` p)) w

-- Analyzer

-- | Tokenize a text with a regular expression for words
scanTextRE :: Text -> Text -> [Word]
scanTextRE wRex = map T.pack . tokenize (T.unpack wRex) . T.unpack

{-
-- | The default analyzer function
scanTextDefault :: Text -> [Word]
scanTextDefault
  = scanTextRE "[^ \t\n\r]*"
-}
