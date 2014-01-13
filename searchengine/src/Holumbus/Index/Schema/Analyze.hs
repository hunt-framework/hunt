module Holumbus.Index.Schema.Analyze
  ( toDocAndWords
  , normalize
  , scanTextRE
  )
where

import           Control.Arrow                   (first)

import           Data.DList                      (DList)
import qualified Data.DList                      as DL
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Text.Regex.XMLSchema.String

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Document        (Document (..),
                                                  DocumentWrapper (..))

import           Holumbus.Common.ApiDocument
import           Holumbus.Index.Schema

import           Holumbus.Index.Schema.Normalize

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

-- | 'ApiDocument' to 'Document' (instance of 'DocumentWrapper') and 'Words' mapping.
--   /Note/: Contexts mentioned in the ApiDoc need to exist.
toDocAndWords :: DocumentWrapper e => Schema -> ApiDocument -> (e, Words)
toDocAndWords s = first wrap . toDocAndWords' s

-- | ApiDocument to Document and Words mapping.
--   /Note/: Contexts mentioned in the ApiDoc need to exist.
toDocAndWords' :: Schema -> ApiDocument -> (Document, Words)
toDocAndWords' schema apiDoc = (doc, ws)
  where
  indexMap = apiDocIndexMap apiDoc
  descrMap = apiDocDescrMap apiDoc
  doc = Document
          { uri   = apiDocUri apiDoc
          , desc  = descrMap
          }
  ws = M.mapWithKey (\context content ->
                        let (ContextSchema _ regex normalizers _ _ cType)
                              = fromJust $ M.lookup context schema
                            (CType _ _ validator _) = cType
                            scan = filter (validate validator) . scanTextRE regex
                        -- XXX: simple concat without nub
                        in toWordList scan (normalize normalizers) content) indexMap

-- | Apply the normalizers to a Word.
normalize :: [CNormalizer] -> Word -> Word
normalize cType = chainFuns . map contextNormalizer $ cType

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

-- | Tokenize a text with a regular expression for words.
--
--  > scanTextRE "[^ \t\n\r]*" == Data.Text.words
scanTextRE :: Text -> Text -> [Word]
scanTextRE wRex = map T.pack . tokenize (T.unpack wRex) . T.unpack
