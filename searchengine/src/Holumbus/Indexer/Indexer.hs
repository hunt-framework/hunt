module Holumbus.Indexer.Indexer where

import           Data.Set                     (Set)
import qualified Data.Set                     as S

import           Holumbus.Utility             (catMaybesSet)

import           Holumbus.Index.Common
import           Holumbus.Index.Index         (Index)
import qualified Holumbus.Index.Index         as Ix
import           Holumbus.DocTable.DocTable   (DocTable)
import qualified Holumbus.DocTable.DocTable   as Dt

-- ----------------------------------------------------------------------------

-- | Generic indexer. A combination of 'Index' and 'DocTable'.
data Indexer it iv i d de
  = Indexer
    { ixIndex    :: Index it iv i
    , ixDocTable :: DocTable d de
    }
 
 -- ----------------------------------------------------------------------------

-- | Returns the number of unique words in the index.
unique                    :: Indexer it iv i d de -> Int
unique                    = Ix.unique . ixIndex

-- | Returns a list of all contexts avaliable in the index.
contexts                  :: Indexer it iv v d de -> [Context]
contexts                  = Ix.contexts . ixIndex

-- | Find a document by 'DocId'.
lookup                    :: (Monad m, Functor m) => Indexer it iv i d de -> DocId -> m de
lookup                    = Dt.lookup . ixDocTable

-- | Find a document by 'URI'.
lookupByURI               :: (Monad m, Functor m) => Indexer it iv i d de -> URI -> m DocId
lookupByURI               = Dt.lookupByURI . ixDocTable

-- | Delete a set if documents by 'DocId'.
deleteDocs                :: Set DocId -> Indexer it iv i d de -> Indexer it iv i d de
deleteDocs     docIds ix  = Indexer { ixIndex  = newIndex
                                    , ixDocTable = newDocTable }
  where
    newDocTable = Dt.difference docIds (ixDocTable ix)
    newIndex    = Ix.deleteDocs docIds (ixIndex    ix)

-- | Delete a set if documents by 'URI'.
deleteDocsByURI           :: Set URI -> Indexer it iv i d de -> Indexer it iv i d de
deleteDocsByURI us ix     = deleteDocs docIds ix
  where
  docIds = catMaybesSet . S.map (lookupByURI ix) $ us
