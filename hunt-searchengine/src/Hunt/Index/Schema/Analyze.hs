-- ----------------------------------------------------------------------------
{- |
  Analyzer for index data.
  Creates raw index data by splitting and normalizing the 'ApiDocument' index data as defined in
  the schema.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.Schema.Analyze
  ( toDocAndWords
  , toDocAndWords'
  , normalize
  , scanTextRE
  )
where

import           Data.DList                  (DList)
import qualified Data.DList                  as DL
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import           Hunt.Common.Document        (Document (..),
                                              DocumentWrapper (..))
import           Hunt.Index.Schema
import           Hunt.Scoring.Score          (Score, toDefScore)

-- ------------------------------------------------------------

-- | Extracts the 'Document' ('DocumentWrapper') and raw index data from an 'ApiDocument' in
--   compliance with the schema.
--
--   /Note/: Contexts mentioned in the 'ApiDocument' need to exist.

toDocAndWords :: DocumentWrapper e => Schema -> ApiDocument -> (e, Score, Words)
toDocAndWords s
    = ( \ (d, dw, ws) -> (wrap d, dw, ws) )
      . toDocAndWords' s

-- | Extracts the 'Document' and raw index data from an 'ApiDocument' in compliance with the schema.
--
--   /Note/: Contexts mentioned in the ApiDoc need to exist.

toDocAndWords' :: Schema -> ApiDocument -> (Document, Score, Words)
toDocAndWords' schema apiDoc
    = (doc, weight, ws)
    where
      indexMap = adIndex apiDoc
      descrMap = adDescr apiDoc
      weight   = adWght  apiDoc
      doc = Document
            { uri   = adUri apiDoc
            , desc  = descrMap
            , wght  = toDefScore weight
            }
      ws = M.mapWithKey
           ( \ context content ->
                 let (ContextSchema rex normalizers _ _ cType)
                         = fromJust $ M.lookup context schema
                     (CType _ defRex validator _)
                         = cType
                     scan
                         = filter (validate validator) . scanTextRE (fromMaybe defRex rex)
                 in
                   toWordList scan (normalize' normalizers) content
           )
           indexMap

-- | Construct a 'WordList' from text using the splitting and normalization function.

toWordList :: (Text -> [Word]) -> (Word -> Word) -> Text -> WordList
toWordList scan norm
    = M.map DL.toList
      . foldr insert M.empty
      . zip [1..]
      . map norm
      . scan
    where
      insert :: (Position, Word) -> Map Word (DList Position) -> Map Word (DList Position)
      insert (p, w)
          = M.alter (return . maybe (DL.singleton p) (`DL.snoc` p)) w

-- | Tokenize a text with a regular expression for words.
--
--  > scanTextRE "[^ \t\n\r]*" == Data.Text.words
--
--   Grammar: <http://www.w3.org/TR/xmlschema11-2/#regexs>
scanTextRE :: RegEx -> Text -> [Word]
scanTextRE = regExTokenize

-- ------------------------------------------------------------
