{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Indexer.TextIndexer where

import           Data.Set                       (Set)
import qualified Data.Set                       as S

import           Holumbus.Utility               (catMaybesSet)

import           Holumbus.DocTable.DocTable     (DocTable)
import qualified Holumbus.DocTable.DocTable     as Dt
import qualified Holumbus.DocTable.HashedDocuments as Hdt
import           Holumbus.Index.Common
import           Holumbus.Index.Common.DocIdMap (DocIdSet, toDocIdSet)
import           Holumbus.Index.Index           (Index)
import qualified Holumbus.Index.Index           as Ix
import           Holumbus.Index.InvertedIndex
import           Holumbus.Index.Proxy.ContextIndex
-- ----------------------------------------------------------------------------

data TextIndexer i dt = TextIndexer 
  { ixx :: (Index i) => ContextIndex i Occurrences
  , dtx :: (Dt.DocTable dt) => dt
  }

emptyInvertedIndexer :: TextIndexer InvertedIndex (Hdt.Documents Document)
emptyInvertedIndexer = TextIndexer 
  { ixx = Ix.empty
  , dtx = Hdt.empty
  }


insert :: (Dt.DValue dt) -> Occurrences -> TextIndexer i dt -> TextIndexer i dt
insert = undefined

-- | Update elements
update :: DocId -> Dt.DValue dt -> Occurrences -> TextIndexer i dt -> TextIndexer i dt
update = undefined

-- | Modify elements
modify :: (Dt.DValue dt -> Dt.DValue dt)
       -> Occurrences -> DocId -> TextIndexer i dt -> TextIndexer i dt
modify = undefined

-- | Delete a set of documents by 'DocId'
delete :: TextIndexer i dt -> DocIdSet -> TextIndexer i dt
delete = undefined
{--delete i dIds
  = modIndexer newIndex newDocTable i
    where
    newIndex    = Ix.deleteDocs dIds $ ixIndex    i
    newDocTable = Dt.difference dIds $ ixDocTable i
--}

-- | Delete a set if documents by 'URI'.
deleteDocsByURI :: Set URI -> TextIndexer i dt -> TextIndexer i dt
deleteDocsByURI = undefined
{--
    deleteDocsByURI us ix
      = delete ix docIds
      where
      docIds = toDocIdSet . catMaybesSet . S.map (lookupByURI ix) $ us
--}

-- ----------------------------------------------------------------------------

-- | See 'Ix.lookup'.
searchPrefixNoCase :: TextIndexer i dt -> Context -> Word -> RawResult
searchPrefixNoCase ti c w = Ix.lookup PrefixNoCase (c,w) (ixx ti) 

-- | See 'Ix.size'.
allWords              :: TextIndexer i => i -> Context -> RawResult
allWords
  = Ix.size . ixIndex

-- | Updates a document by 'DocId'.
update                :: (TextIndexer i, de ~ Dt.DValue (IxDocTable i)) =>
                         DocId -> de -> Words
                         -> i
                         -> i
update docId doc' w ix
  = insert doc' w ix'
  where
  ix' = Ixx.delete ix (IS.singleton docId)

-- | Insert a document.
insert                :: (TextIndexer ix, de ~ Dt.DValue (IxDocTable ix)) =>
                         de -> Words
                         -> ix
                         -> ix
insert doc' wrds ix
  = modIndexer newIndex newDocTable ix
  where
  di = ixDocTable ix
  ii = ixIndex    ix
  (did, newDocTable) = Dt.insert di doc'
  newIndex           = addWords wrds did ii


-- | Modify a document and add words (occurrences for that document) to the index.
modify                :: (TextIndexer i, de ~ Dt.DValue (IxDocTable i)) =>
                         (de -> de) -> Words -> DocId
                         -> i
                         -> i
modify f wrds dId ix
  = modIndexer newIndex newDocTable ix
  where
  newDocTable = Dt.adjust f dId   (ixDocTable ix)
  newIndex    = addWords wrds dId (ixIndex ix)


-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (TextIndexer i {-, DocumentWrapper (Dt.DValue (IxDocTable i))-}) =>
                         Description -> Words -> DocId -> i -> i
modifyWithDescription descr wrds dId ix
  = modIndexer newIndex newDocTable ix
  where
  newDocTable = Dt.adjust mergeDescr dId $ ixDocTable ix
  newIndex    = addWords wrds dId $ ixIndex ix
  -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
  mergeDescr  = Doc.update (\d' -> d'{ desc = flip M.union (desc d') descr })

-- ----------------------------------------------------------------------------

-- Helper functions

-- | Add words for a document to the 'Index'.
addWords              :: TextIndex i => Words -> DocId -> i -> i
addWords wrds dId i
  = M.foldrWithKey (\c wl acc ->
      M.foldrWithKey (\w ps acc' ->
        Ix.insert c w (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws

-- Specific to Indexes with Document DocTable values
{-
addDocDescription         :: Description -> DocId -> Indexer it iv i d Document -> Indexer it iv i d Document
addDocDescription descr did (Indexer i d)
  = Indexer i (Dt.modify mergeDescr did d)
  where
  mergeDescr doc = doc{ desc = M.union (desc doc) descr }
-}
----------------------------------------------------------------------------
