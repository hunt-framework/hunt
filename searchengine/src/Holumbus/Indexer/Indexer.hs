module Holumbus.Indexer.Indexer where

import           Data.Set                     (Set)
import qualified Data.Set                     as S

import           Holumbus.Utility             (catMaybesSet)

import           Holumbus.Index.Common
import           Holumbus.Index.Index         (Index)
import qualified Holumbus.Index.Index         as Ix
import           Holumbus.DocTable.DocTable   (DocTable)
import qualified Holumbus.DocTable.DocTable   as Dt


-- | Generic indexer. A combination of index and doc table.
data Indexer it iv i d de
  = Indexer
    { ixIndex    :: Index it iv i
    , ixDocTable :: DocTable d de
    }

-- | Find a document by 'DocId'.
lookupById                :: (Monad m, Functor m) => Indexer it iv i d de -> DocId -> m de
lookupById                = Dt.lookupById . ixDocTable

-- | Find a document by 'URI'.
lookupByURI               :: (Monad m, Functor m) => Indexer it iv i d de -> URI -> m DocId
lookupByURI               = Dt.lookupByURI . ixDocTable

-- | Delete a set if documents by 'DocId'.
deleteDocsById            :: Set DocId -> Indexer it iv i d de -> Indexer it iv i d de
deleteDocsById docIds ix  = Indexer { ixIndex  = newIndex
                                    , ixDocTable = newDocTable }
  where
    newDocTable = Dt.differenceById docIds (ixDocTable ix)
    newIndex    = Ix.deleteDocsById docIds (ixIndex    ix)

-- | Delete a set if documents by 'URI'.
deleteDocsByURI           :: Set URI -> Indexer it iv i d de -> Indexer it iv i d de
deleteDocsByURI us ix     = deleteDocsById docIds ix
  where
  docIds = catMaybesSet . S.map (lookupByURI ix) $ us
