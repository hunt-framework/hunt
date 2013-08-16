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

-- ----------------------------------------------------------------------------

-- | TextIndexer with 'Index' implementation, 'DocTable' implementation and element type parameter.
--   Uses 'Textual' and 'Occurrences' as index and value type.
type TextIndexer i d de   = Indexer Textual Occurrences i d de

-- ----------------------------------------------------------------------------

-- index functions

-- | See 'Ix.lookup'.
searchPrefixNoCase        :: Indexer Textual iv i d de -> Context -> Text -> RawResult
searchPrefixNoCase (Indexer ix _dx) = Ix.lookup PrefixNoCase ix

-- | See 'Ix.size'.
allWords                  :: Indexer Textual iv i d de -> Context -> RawResult
allWords                  = Ix.size . ixIndex

-- | Update a document - same as delete and insert.
updateDoc                 :: DocId -> de -> Words -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
updateDoc docId doc w     = insertDoc doc w . deleteDocs (S.singleton docId)

-- | Insert a document.
insertDoc                 :: de -> Words -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
insertDoc doc wrds ix     = ix { ixIndex    = newIndex
                               , ixDocTable = newDocTable }
  where
  (dId, newDocTable) = Dt.insert (ixDocTable ix) doc
  newIndex           = addWords wrds dId $ ixIndex ix

-- | Modify a document and add words (occurrences for that document) to the index.
modify                    :: (de -> de) -> Words -> DocId -> Indexer it Occurrences i d de -> Indexer it Occurrences i d de
modify f wrds dId ix
  = ix { ixIndex    = newIndex
       , ixDocTable = newDocTable }
  where
  newDocTable = Dt.adjust f dId   $ ixDocTable ix
  newIndex    = addWords wrds dId $ ixIndex ix

-- | Modify the description of a document and add words (occurrences for that document) to the index.
modifyWithDescription     :: Description -> Words -> DocId -> Indexer it Occurrences i d Document -> Indexer it Occurrences i d Document
modifyWithDescription descr wrds dId ix
  = ix { ixIndex    = newIndex
       , ixDocTable = newDocTable }
  where
  newDocTable    = Dt.adjust mergeDescr dId $ ixDocTable ix
  newIndex       = addWords wrds dId $ ixIndex ix
  mergeDescr doc = doc{ desc = desc doc `M.union` descr }

-- ----------------------------------------------------------------------------

-- Helper functions

-- | Add words for a document to the 'Index'.
addWords                  :: Words -> DocId -> Index it Occurrences i -> Index it Occurrences i
addWords wrds dId i = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Ix.insert c w (mkOccs dId ps) acc') acc wl) i wrds
  where
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
