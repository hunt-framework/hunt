module Holumbus.Indexer.TextIndexer 
  ( module Holumbus.Indexer.Indexer
  , TextIndexer
  , searchPrefixNoCase
  , allWords
  , updateDoc
  , insertDoc
  , modify
  , modifyWithDescription
  )
where

import qualified Data.Map                          as M
import qualified Data.Set                          as S
import           Data.Text                         (Text)

import qualified Holumbus.DocTable.DocTable        as Dt
import           Holumbus.Index.Index              (Index)
import qualified Holumbus.Index.Index              as Ix

import           Holumbus.Index.Common
import           Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Indexer.Indexer


type TextIndexer i d de   = Indexer Textual Occurrences i d de

-- index functions

searchPrefixNoCase        :: Indexer Textual iv i d de -> Context -> Text -> RawResult
searchPrefixNoCase (Indexer ix _dx) = Ix.lookup PrefixNoCase ix

allWords                  :: Indexer Textual iv i d de -> Context -> RawResult
allWords                  = Ix.size . ixIndex

updateDoc                 :: DocId -> de -> Words -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
updateDoc docId doc w     = insertDoc doc w . deleteDocs (S.singleton docId)

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
  newDocTable = Dt.adjust f dId   $ ixDocTable ix
  newIndex    = addWords wrds dId $ ixIndex ix

modifyWithDescription     :: Description -> Words -> DocId -> Indexer it Occurrences i d Document -> Indexer it Occurrences i d Document
modifyWithDescription descr wrds dId ix
  = ix { ixIndex    = newIndex
       , ixDocTable = newDocTable }
  where
  newDocTable    = Dt.adjust mergeDescr dId $ ixDocTable ix
  newIndex       = addWords wrds dId $ ixIndex ix
  mergeDescr doc = doc{ desc = desc doc `M.union` descr }

-- Helper functions

addWords                  :: Words -> DocId -> Index it Occurrences i -> Index it Occurrences i
addWords wrds dId i = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Ix.insert c w (mkOccs dId ps) acc') acc wl) i wrds

mkOccs                    :: DocId -> [Position] -> Occurrences
mkOccs did pl = positionsIntoOccs did pl Occ.empty

positionsIntoOccs         :: DocId -> [Position] -> Occurrences -> Occurrences
positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws

-- Specific to Indexes with Document DocTable values
{-
addDocDescription         :: Description -> DocId -> Indexer it iv i d Document -> Indexer it iv i d Document
addDocDescription descr did (Indexer i d)
  = Indexer i (Dt.modify mergeDescr did d)
  where
  mergeDescr doc = doc{ desc = M.union (desc doc) descr }
-}
