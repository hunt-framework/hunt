module Holumbus.Indexer.TextIndexer 
  ( module Holumbus.Indexer.Indexer
  , TextIndexer
  , searchPrefixNoCase
  , allWords
  )
where

import           Data.Text

import           Holumbus.Index.Common
import           Holumbus.Indexer.Indexer
import qualified Holumbus.Index.Index as Ix

type TextIndexer i d de = Indexer Textual Occurrences i d de

-- index functions
searchPrefixNoCase        :: Indexer Textual iv i d de -> Context -> Text -> RawResult
searchPrefixNoCase (Indexer ix _dx) c w = Ix.lookup PrefixNoCase ix c w

allWords                  :: Indexer Textual iv i d de -> Context -> RawResult
allWords                  = Ix.allWords . ixIndex


