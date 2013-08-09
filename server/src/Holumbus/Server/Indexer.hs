module Holumbus.Server.Indexer where

import           Data.Set                     (Set)
import qualified Data.Set                     as S
import qualified Data.Map                     as M
import           Data.Text                    (Text)

import           Holumbus.Index.Common        (Context, URI, RawResult
                                              , Position, Occurrences, emptyOccurrences, insertOccurrence
                                              , DocId, Document(..), Description)

import           Holumbus.Server.Common

import           Holumbus.Utility             (catMaybesSet)

import           Holumbus.Index.Index         (Index)
import qualified Holumbus.Index.Index         as Ix
import           Holumbus.Index.DocTable      (DocTable)
import qualified Holumbus.Index.DocTable      as Dt


-- generic indexer - combination of an index and a doc table
data Indexer it iv i d de
  = Indexer
    { ixIndex    :: Index it iv i
    , ixDocTable :: DocTable d de
    }

-- index functions
searchPrefixNoCase        :: Indexer Ix.Textual iv i d de -> Context -> Text -> RawResult
searchPrefixNoCase (Indexer ix _dx) c w = Ix.lookup Ix.PrefixNoCase ix c w

allWords                  :: Indexer Ix.Textual iv i d de -> Context -> RawResult
allWords                  = Ix.allWords . ixIndex

-- doctable functions
lookupById                :: (Monad m, Functor m) => Indexer it iv i d de -> DocId -> m de
lookupById                = Dt.lookupById . ixDocTable

lookupByURI               :: (Monad m, Functor m) => Indexer it iv i d de -> URI -> m DocId
lookupByURI               = Dt.lookupByURI . ixDocTable

deleteDocsById            :: Set DocId -> Indexer it iv i d de -> Indexer it iv i d de
deleteDocsById docIds ix  = Indexer { ixIndex  = newIndex
                                    , ixDocTable = newDocTable }
  where
    newDocTable = Dt.deleteById     docIds (ixDocTable ix)
    newIndex    = Ix.deleteDocsById docIds (ixIndex    ix)

deleteDocsByURI           :: Set URI -> Indexer it iv i d de -> Indexer it iv i d de
deleteDocsByURI us ix     = deleteDocsById docIds ix
  where
  docIds = catMaybesSet . S.map (lookupByURI ix) $ us

-- TODO: move?
-- Specific to Indexes with Occurrences values

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