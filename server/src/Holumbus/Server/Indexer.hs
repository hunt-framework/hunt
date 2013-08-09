module Holumbus.Server.Indexer where

import Data.Set                               (Set)
import qualified Data.Set                     as S
import qualified Data.Map                     as M
import           Data.Text                    (Text)

import           Holumbus.Index.Common        (Context, URI, RawResult
                                              , Position, Occurrences, emptyOccurrences, insertOccurrence
                                              , DocId)

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

updateDoc                 :: DocId -> de -> Words -> Indexer it Occurrences iv d de -> Indexer it Occurrences iv d de
updateDoc docId doc w     = insertDoc doc w . deleteDocsById (S.singleton docId)

insertDoc                 :: de -> Words -> Indexer it Occurrences iv d de -> Indexer it Occurrences iv d de
insertDoc doc wrds ix     = ix { ixIndex    = newIndex
                               , ixDocTable = newDocTable }
  where
  (dId, newDocTable) = Dt.insertDoc (ixDocTable ix) doc
  newIndex           = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Ix.insertOccurrences c w (mkOccs dId ps) acc') acc wl) (ixIndex ix) wrds

  mkOccs :: DocId -> [Position] -> Occurrences
  mkOccs did pl = insertPositions did pl emptyOccurrences

  insertPositions :: DocId -> [Position] -> Occurrences -> Occurrences
  insertPositions docId ws os = foldr (insertOccurrence docId) os ws
