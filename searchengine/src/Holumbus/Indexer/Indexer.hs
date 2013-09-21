{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.Indexer.Indexer where

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

class (Index (IxIndex i), DocTable (IxDocTable i)) =>
      Indexer i where
    type IxIndex    i :: *
    type IxDocTable i :: *
    type IxElem     i :: * -- FIXME: meh
                           -- maybe a class ToWords a where toWords :: a -> Words?

    -- | Access 'Index' directly
    ixIndex     :: i -> IxIndex i

    -- | Access 'DocTable' directly
    ixDocTable  :: i -> IxDocTable i

    -- | Insert elements to 'Index' and 'DocTable'
    insert      :: Dt.DValue (IxDocTable i) -> IxElem i -> i -> i

    -- | Update elements
    update      :: DocId -> Dt.DValue (IxDocTable i) -> IxElem i -> i -> i

    -- | Modify elements
    modify      :: (Dt.DValue (IxDocTable i) -> Dt.DValue (IxDocTable i))
                   -> IxElem i -> DocId -> i -> i

    -- | Delete a set of documents by 'DocId'
    delete      :: i -> DocIdSet -> i
    delete i dIds
      = modIndexer newIndex newDocTable i
      where
      newIndex    = Ix.deleteDocs dIds $ ixIndex    i
      newDocTable = Dt.difference dIds $ ixDocTable i

    -- | Delete a set if documents by 'URI'.
    deleteDocsByURI       :: Set URI -> i -> i
    deleteDocsByURI us ix
      = delete ix docIds
      where
      docIds = toDocIdSet . catMaybesSet . S.map (lookupByURI ix) $ us

    -- | Returns the number of unique words in the index.
    unique      :: i -> Int
    unique
      = Ix.unique . ixIndex

    -- | Returns a list of all contexts avaliable in the index.
    contexts    :: i -> [Context]
    contexts
      = Ix.contexts . ixIndex

    -- | Find a document by 'DocId'.
    lookup      :: (Monad m, Functor m) => i -> DocId -> m (Dt.DValue (IxDocTable i))
    lookup
      = Dt.lookup . ixDocTable

    -- | Find a document by 'URI'.
    lookupByURI :: (Monad m, Functor m) => i -> URI -> m DocId
    lookupByURI
      = Dt.lookupByURI . ixDocTable

    -- | Number of documents.
    size        :: i -> Int
    size
      = Dt.size . ixDocTable

    -- Functions to enable writing default implementations like 'delete'

    -- | Replace the 'Index' .
    modIndex    :: IxIndex i    -> i -> i

    -- | Replace the  'DocTable'.
    modDocTable :: IxDocTable i -> i -> i

    -- | Replace the 'Index' and 'DocTable'.
    modIndexer  :: IxIndex i -> IxDocTable i -> i -> i
    modIndexer ii di ix
      = modDocTable di . modIndex ii $ ix

-- ----------------------------------------------------------------------------
