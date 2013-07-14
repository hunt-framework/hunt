module Holumbus.Server.Indexer where

import Data.Set                               (Set)
import qualified Data.Set                     as S
import qualified Data.Map                     as M
import           Data.Text                    (Text)

import           Holumbus.Index.Common        (Context, URI, RawResult
                                              , Position, Occurrences, emptyOccurrences, insertOccurrence
                                              , DocId, Document)

import           Holumbus.Server.Common

import           Holumbus.Utility             (catMaybesSet)

import           Holumbus.Index.Index         (Index)
import qualified Holumbus.Index.Index         as Ix
import           Holumbus.Index.DocTable      (DocTable)
import qualified Holumbus.Index.DocTable      as Dt


-- generic indexer - combination of an index and a doc table
data Indexer i d
  = Indexer
    { ixIndex    :: Index    i
    , ixDocTable :: DocTable d
    }

-- index functions
searchPrefixNoCase        :: Indexer i d -> Context -> Text -> RawResult
searchPrefixNoCase        = Ix.prefixNoCase . ixIndex

allWords                  :: Indexer i d -> Context -> RawResult
allWords                  = Ix.allWords . ixIndex

-- doctable functions
lookupById                :: Monad m => Indexer i d -> DocId -> m Document
lookupById                = Dt.lookupById . ixDocTable

lookupByURI               :: Monad m => Indexer i d -> URI -> m DocId
lookupByURI               = Dt.lookupByURI . ixDocTable

deleteDocsById            :: Set DocId -> Indexer i d -> Indexer i d
deleteDocsById docIds ix  = Indexer { ixIndex  = newIndex
                                    , ixDocTable = newDocTable }
  where
    newDocTable = Dt.deleteById     docIds (ixDocTable ix)
    newIndex    = Ix.deleteDocsById docIds (ixIndex    ix)

deleteDocsByURI           :: Set URI -> Indexer i d -> Indexer i d
deleteDocsByURI us ix     = deleteDocsById docIds ix
  where
  docIds = catMaybesSet . S.map (lookupByURI ix) $ us

updateDoc                 :: DocId -> Document -> Words -> Indexer i d -> Indexer i d
updateDoc docId doc w     = insertDoc doc w . deleteDocsById (S.singleton docId)

insertDoc                 :: Document -> Words -> Indexer i d -> Indexer i d
insertDoc doc wrds ix     = ix { ixIndex    = newIndex
                               , ixDocTable = newDocTable }
  where
  (dId, newDocTable) = Dt.insertDoc (ixDocTable ix) doc
  newIndex           = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Ix.insertOccurrences c w (mkOccs dId ps) acc') acc wl) (ixIndex ix) wrds

  mkOccs :: DocId -> [Position] -> Occurrences
  mkOccs did pl = insertPositions did pl emptyOccurrences

  insertPositions :: DocId -> [Position] -> Occurrences -> Occurrences
  insertPositions docId ws os = foldr (insertOccurrence docId) os ws