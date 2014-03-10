module Hunt.Index.Schema.Analyze
  ( toDocAndWords
  , toDocAndWords'
  , normalize
  , scanTextRE
  )
where

import           Control.Arrow               (first)

import           Data.DList                  (DList)
import qualified Data.DList                  as DL
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Text.Regex.XMLSchema.String

import           Hunt.Common.BasicTypes
import           Hunt.Common.Document        (Document (..),
                                              DocumentWrapper (..))

import           Hunt.Common.ApiDocument
import           Hunt.Index.Schema

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
  indexMap = adIndex apiDoc
  descrMap = adDescr apiDoc
  doc = Document
    { uri   = adUri apiDoc
    , desc  = descrMap
    }
  ws = M.mapWithKey
        (\context content ->
            let (ContextSchema rex normalizers _ _ cType)
                  = fromJust $ M.lookup context schema
                (CType _ defRex validator _) = cType
                scan = filter (validate validator) . scanTextRE (fromMaybe defRex rex)
            -- XXX: simple concat without nub
            in toWordList scan (normalize normalizers) content)
        indexMap

-- | Apply the normalizers to a Word.
normalize :: [CNormalizer] -> Word -> Word
normalize ns  = foldl (\f2 (CNormalizer _ f) -> f.f2) id $ ns

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
scanTextRE :: CRegex -> Text -> [Word]
scanTextRE wRex = map T.pack . tokenize (T.unpack wRex) . T.unpack
