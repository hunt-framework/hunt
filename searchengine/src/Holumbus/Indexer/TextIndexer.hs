module Holumbus.Indexer.TextIndexer 
  ( module Holumbus.Indexer.Indexer
  , TextIndexer
  , newTextIndexer
  , searchPrefixNoCase
  , allWords
  , modifyWithDescription
  )
where

import qualified Data.Map                          as M
import qualified Data.Set                          as S
import           Data.Text                         (Text)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt
import           Holumbus.Index.Index              (Index)
import qualified Holumbus.Index.Index              as Ix

import           Holumbus.Index.Common
import           Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Indexer.Indexer
import qualified Holumbus.Indexer.Indexer          as Ixx

-- ----------------------------------------------------------------------------

-- | TextIndexer with 'Index' implementation, 'DocTable' implementation and element type parameter.
--   Uses 'Textual' and 'Occurrences' as index and value type.
type TextIndexer i d de   = Indexer Words Textual Occurrences i d de

newTextIndexer :: Index Textual Occurrences i -> DocTable d de -> TextIndexer i d de
newTextIndexer ix dt = (newIndexer ix dt) 
  { _insert = \doc ws -> insertDoc doc ws ix dt
  , _update = \docId doc w -> updateDoc docId doc w ix dt
  , _modify = \f ws docId -> modify' f ws docId ix dt
  } 
 
-- ----------------------------------------------------------------------------

-- index functions

-- | See 'Ix.lookup'.
searchPrefixNoCase        :: TextIndexer i d de -> Context -> Text -> RawResult
searchPrefixNoCase (Indexer ix _dx _ _ _ _) = Ix.lookup PrefixNoCase ix

-- | See 'Ix.size'.
allWords                  :: TextIndexer i d de -> Context -> RawResult
allWords                  = Ix.size . ixIndex

-- | Update a document - same as delete and insert.
updateDoc                    :: DocId -> de -> Words 
                             -> Index Textual Occurrences i -> DocTable d de
                             -> TextIndexer i d de
updateDoc docId doc w ix dt  = insertDoc doc w nIx nDt
  where
  (Indexer nIx nDt _ _ _ _ ) = Ixx.delete tIx (S.singleton docId)
  tIx = newIndexer ix dt

-- | Insert a document.
insertDoc                 :: de -> Words 
                          -> Index Textual Occurrences i -> DocTable d de
                          -> TextIndexer i d de
insertDoc doc wrds ix dt  = newTextIndexer  newIndex newDocTable
  where
  (dId, newDocTable) = Dt.insert dt doc
  newIndex           = addWords wrds dId ix

-- | Modify a document and add words (occurrences for that document) to the index.
modify'                   :: (de -> de) -> Words -> DocId 
                          -> Index Textual Occurrences i -> DocTable d de
                          -> TextIndexer i d de
modify' f wrds dId ix dt = newTextIndexer newIndex newDocTable
  where
  newDocTable = Dt.adjust f dId   $ dt
  newIndex    = addWords wrds dId $ ix

-- | Modify the description of a document and add words (occurrences for that document) to the index.
modifyWithDescription     :: Description -> Words -> DocId -> TextIndexer i d Document -> TextIndexer i d Document
modifyWithDescription descr wrds dId ix
  = ix { _ixIndex    = newIndex
       , _ixDocTable = newDocTable }
  where
  newDocTable    = Dt.adjust mergeDescr dId $ ixDocTable ix
  newIndex       = addWords wrds dId $ ixIndex ix
  -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
  mergeDescr doc = doc{ desc = flip M.union (desc doc) descr }

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
