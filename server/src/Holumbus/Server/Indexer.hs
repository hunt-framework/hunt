module Holumbus.Server.Indexer where

import           Data.Maybe                   (fromJust)
import Data.Set                               (Set)
import qualified Data.Set                     as S
import qualified Data.Map                     as M
import           Data.Text                    (Text)

import           Holumbus.Index.Common        (Context, URI, RawResult
                                              , Position, Occurrences, emptyOccurrences, insertOccurrence
                                              , DocId, Document
                                              , HolIndex, HolDocuments)
import qualified Holumbus.Index.Common        as Co

import           Holumbus.Server.Common


-- generic indexer - combination of an index and a doc table
data Indexer i d
  = Indexer
    { ixIndex    :: i
    , ixDocTable :: d
    }

-- index functions
searchPrefixNoCase        :: (HolIndex i, HolDocuments d)
                          => Indexer i d -> Context -> Text -> RawResult
searchPrefixNoCase        = Co.prefixNoCase . ixIndex

allWords                  :: HolIndex i
                          => Indexer i d -> Context -> RawResult
allWords                  = Co.allWords . ixIndex

-- doctable functions
lookupById                :: (HolIndex i, HolDocuments d, Monad m)
                          => Indexer i d -> DocId -> m Document
lookupById                = Co.lookupById . ixDocTable

lookupByURI               :: (HolIndex i, HolDocuments d, Monad m)
                          => Indexer i d -> URI -> m DocId
lookupByURI               = Co.lookupByURI . ixDocTable

deleteDocsById            :: (HolIndex i, HolDocuments d)
                          => Set DocId -> Indexer i d -> Indexer i d
deleteDocsById docIds ix  = Indexer { ixIndex     = newIndex
                                    , ixDocTable = newDocTable }
  where
    newDocTable = Co.deleteById     docIds (ixDocTable ix)
    newIndex    = Co.deleteDocsById docIds (ixIndex    ix)

deleteDocsByURI           :: (HolIndex i, HolDocuments d)
                          => Set URI -> Indexer i d -> Indexer i d
deleteDocsByURI us ix     = deleteDocsById docIds ix
  where
  docIds = catMaybesSet . S.map (lookupByURI ix) $ us

updateDoc                 :: (HolIndex i, HolDocuments d)
                          => DocId -> Document -> Words -> Indexer i d -> Indexer i d
updateDoc docId doc w     = insertDoc doc w . deleteDocsById (S.singleton docId)

insertDoc                 :: (HolIndex i, HolDocuments d)
                          => Document -> Words -> Indexer i d -> Indexer i d
insertDoc doc wrds ix     = ix { ixIndex    = newIndex
                               , ixDocTable = newDocTable }
  where
  (dId, newDocTable) = Co.insertDoc (ixDocTable ix) doc
  newIndex           = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Co.insertOccurrences c w (mkOccs dId ps) acc') acc wl) (ixIndex ix) wrds

  mkOccs :: DocId -> [Position] -> Occurrences
  mkOccs did pl = insertPositions did pl emptyOccurrences

  insertPositions :: DocId -> [Position] -> Occurrences -> Occurrences
  insertPositions docId ws os = foldr (insertOccurrence docId) os ws


-- Utitlity

-- | Data.Maybe.catMaybes on a Set instead of a List.
catMaybesSet :: Ord a => Set (Maybe a) -> Set a
catMaybesSet = S.map fromJust . S.delete Nothing