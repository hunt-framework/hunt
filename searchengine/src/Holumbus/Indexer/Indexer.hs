module Holumbus.Indexer.Indexer where

import           Control.DeepSeq

import           Data.Set                       (Set)
import qualified Data.Set                       as S

import           Holumbus.Utility               (catMaybesSet)

import           Holumbus.DocTable.DocTable     (DocTable)
import qualified Holumbus.DocTable.DocTable     as Dt
import           Holumbus.Index.Common
import           Holumbus.Index.Common.DocIdMap (DocIdSet, toDocIdSet)
import           Holumbus.Index.Index           (Index)
import qualified Holumbus.Index.Index           as Ix

-- ----------------------------------------------------------------------------

-- | Access 'index' directly
ixIndex :: Indexer elem it iv i d de -> Index it iv i
ixIndex = _ixIndex

-- | Access 'DocTable' directly
ixDocTable :: Indexer elem it iv i d de -> DocTable d de
ixDocTable = _ixDocTable

-- | Insert elements to 'Index' and 'DocTable'
insert :: Indexer elem it iv i d de -> de -> elem -> Indexer elem it iv i d de
insert = _insert

-- | Update elements
update :: Indexer elem it iv i d de -> DocId -> de -> elem -> Indexer elem it iv i d de
update = _update

-- | Modify elements
modify :: Indexer elem it iv i d de -> (de -> de) -> elem -> DocId -> Indexer elem it iv i d de
modify = _modify

-- | Delete a set of documents by 'DocId'
delete :: Indexer elem it iv i d de -> DocIdSet -> Indexer elem it iv i d de
delete = _delete

-- | Delete a set if documents by 'URI'.
deleteDocsByURI           :: Set URI -> Indexer elem it iv i d de -> Indexer elem it iv i d de
deleteDocsByURI us ix     = _delete ix docIds
  where
  docIds = toDocIdSet . catMaybesSet . S.map (lookupByURI ix) $ us

instance NFData (Indexer elem it iv i d de) where
  rnf Indexer{} = rnf _ixIndex `seq` rnf ixDocTable

 -- ----------------------------------------------------------------------------

-- | Returns the number of unique words in the index.
unique                    :: Indexer elem it iv i d de -> Int
unique                    = Ix.unique . ixIndex

-- | Returns a list of all contexts avaliable in the index.
contexts                  :: Indexer elem it iv v d de -> [Context]
contexts                  = Ix.contexts . ixIndex

-- | Find a document by 'DocId'.
lookup                    :: (Monad m, Functor m) => Indexer elem it iv i d de -> DocId -> m de
lookup                    = Dt.lookup . ixDocTable

-- | Find a document by 'URI'.
lookupByURI               :: (Monad m, Functor m) => Indexer elem it iv i d de -> URI -> m DocId
lookupByURI               = Dt.lookupByURI . ixDocTable

-- ----------------------------------------------------------------------------

-- | Generic indexer. A combination of 'Index' and 'DocTable'.
data Indexer elem it iv i d de
  = Indexer
    { _ixIndex    :: Index it iv i
    , _ixDocTable :: DocTable d de
    , _insert     :: de -> elem -> Indexer elem it iv i d de
    , _update     :: DocId -> de -> elem -> Indexer elem it iv i d de
    , _modify     :: (de -> de) -> elem -> DocId -> Indexer elem it iv i d de
    , _delete     :: DocIdSet -> Indexer elem it iv i d de
    }

-- ----------------------------------------------------------------------------

newIndexer :: Index it0 iv0 i0
           -> DocTable d0 de0
           -> Indexer elem0 it0 iv0 i0 d0 de0
newIndexer ix dt = Indexer
  { _ixIndex     = ix
  , _ixDocTable  = dt
  , _insert      = undefined -- XXX add default impl here
  , _update      = undefined
  , _modify      = undefined
  , _delete      = \ids -> newIndexer (Ix.deleteDocs ids ix) (Dt.difference ids dt)
  }


