module Holumbus.Indexer.TextIndexer 
  ( module Holumbus.Indexer.Indexer
  , TextIndexer
  , searchPrefixNoCase
  , allWords
  , updateDoc
  , insertDoc
  , modifyWithDescription
  )
where

import           Data.Text                    (Text)
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import qualified Data.Map                     as M


import           Holumbus.Index.Common        ( URI, Words
                                              , Position, Occurrences, emptyOccurrences, insertOccurrence
                                              , DocId, Document(..), Description)

import           Holumbus.Utility             (catMaybesSet)

import           Holumbus.Index.Index         (Index)
import qualified Holumbus.Index.Index         as Ix
import           Holumbus.DocTable.DocTable   (DocTable)
import qualified Holumbus.DocTable.DocTable   as Dt



import           Holumbus.Index.Common
import           Holumbus.Indexer.Indexer
import qualified Holumbus.Index.Index as Ix

type TextIndexer i d de = Indexer Textual Occurrences i d de

-- index functions
searchPrefixNoCase        :: Indexer Textual iv i d de -> Context -> Text -> RawResult
searchPrefixNoCase (Indexer ix _dx) c w = Ix.lookup PrefixNoCase ix c w

allWords                  :: Indexer Textual iv i d de -> Context -> RawResult
allWords                  = Ix.allWords . ixIndex

updateDoc                 :: DocId -> de -> Words -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
updateDoc docId doc w     = insertDoc doc w . deleteDocsById (S.singleton docId)

insertDoc                 :: de -> Words -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
insertDoc doc wrds ix     = ix { ixIndex    = newIndex
                               , ixDocTable = newDocTable }
  where
  (dId, newDocTable) = Dt.insert (ixDocTable ix) doc
  newIndex           = addWords wrds dId $ ixIndex ix

modify                    :: (de -> de) -> Words -> DocId -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
modify f wrds dId ix
  = ix { ixIndex    = newIndex
       , ixDocTable = newDocTable }
  where
  newDocTable = Dt.modify f dId   $ ixDocTable ix
  newIndex    = addWords wrds dId $ ixIndex ix

modifyWithDescription     :: Description -> Words -> DocId -> Indexer it Occurrences i d Document -> Indexer it Occurrences i d Document
modifyWithDescription descr wrds dId ix
  = ix { ixIndex    = newIndex
       , ixDocTable = newDocTable }
  where
  newDocTable = Dt.modify mergeDescr dId   $ ixDocTable ix
  newIndex    = addWords wrds dId $ ixIndex ix
  mergeDescr doc = doc{ desc = M.union (desc doc) descr }

-- Helper functions

addWords                  :: Words -> DocId -> Index it Occurrences i -> Index it Occurrences i
addWords wrds dId i = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Ix.insert c w (mkOccs dId ps) acc') acc wl) i wrds

mkOccs                    :: DocId -> [Position] -> Occurrences
mkOccs did pl = positionsIntoOccs did pl emptyOccurrences

positionsIntoOccs         :: DocId -> [Position] -> Occurrences -> Occurrences
positionsIntoOccs docId ws os = foldr (insertOccurrence docId) os ws

-- Specific to Indexes with Document DocTable values
{-
addDocDescription         :: Description -> DocId -> Indexer it iv i d Document -> Indexer it iv i d Document
addDocDescription descr did (Indexer i d)
  = Indexer i (Dt.modify mergeDescr did d)
  where
  mergeDescr doc = doc{ desc = M.union (desc doc) descr }
-}
