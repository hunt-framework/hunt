module Holumbus.Indexer.Indexer where

import qualified Data.Map                     as M
import           Data.Set                     (Set)
import qualified Data.Set                     as S

import           Holumbus.Index.Common        ( URI, Words
                                              , DocId, Document(..), Description)

import           Holumbus.Utility             (catMaybesSet)

import           Holumbus.Index.Index         (Index)
import qualified Holumbus.Index.Index         as Ix
import           Holumbus.DocTable.DocTable   (DocTable)
import qualified Holumbus.DocTable.DocTable   as Dt


-- generic indexer - combination of an index and a doc table
data Indexer it iv i d de
  = Indexer
    { ixIndex    :: Index it iv i
    , ixDocTable :: DocTable d de
    }

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


