module Holumbus.Server.Analyzer
  (
  toDocAndWords
  )
where

import           Data.Map                               (Map)
import qualified Data.Map                               as M
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Char                              (isAlphaNum)

import           Data.DList                             (DList)
import qualified Data.DList                             as DL

import           Holumbus.Index.Common                  (Document(..), Word, Position)

import           Holumbus.Server.Common


-- | ApiDocument to Document and Words mapping.
toDocAndWords :: ApiDocument -> (Document, Words)
toDocAndWords apiDoc = (doc, ws)
  where
  mapping = apiDocMapping apiDoc
  doc = Document
          { uri   = apiDocUri apiDoc
          , desc  = dc
          }
  ws = M.map (toWordList . contentRaw) . M.filter (isIndexMetaData       . contentMetadata) $ mapping
  dc = M.map contentRaw                . M.filter (isDescriptionMetaData . contentMetadata) $ mapping

isDescriptionMetaData :: ContentMetadata -> Bool
isDescriptionMetaData (ContentMetadata _ b) = b

isIndexMetaData       :: ContentMetadata -> Bool
isIndexMetaData       (ContentMetadata b _) = b


toWordList :: Text -> WordList
toWordList = M.map DL.toList . foldr insert M.empty . scanText
  where
  insert :: (Position, Word) -> Map Word (DList Position) -> Map Word (DList Position)
  insert (p, w)
    = M.alter (return . maybe (DL.singleton p) (`DL.snoc` p)) w

scanText :: Text -> [(Position, Text)]
scanText
  = zip [0..] . T.words . T.map (\c -> if isAlphaNum c then c else ' ') -- XXX: keep isAlphaNum?