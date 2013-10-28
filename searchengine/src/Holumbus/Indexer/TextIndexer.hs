{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Indexer.TextIndexer where

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M

--import qualified Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Utility                  (catMaybesSet)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt
import qualified Holumbus.DocTable.HashedDocuments as Hdt
import           Holumbus.Index.Common
import           Holumbus.Index.Common.DocIdMap    (DocIdSet, toDocIdSet)
--import           Holumbus.Index.Index              (Index)
import qualified Holumbus.Index.Index              as Ix
import qualified Holumbus.Index.TextIndex          as TIx
import           Holumbus.Index.InvertedIndex
import           Holumbus.Index.Proxy.ContextIndex
import qualified Holumbus.Index.Common.Document    as Doc

-- ----------------------------------------------------------------------------

type TextIndexer i dt = (TIx.TextIndex i Occurrences, DocTable dt)

data Indexer i dt = Indexer
  { ixIndex    :: ContextIndex i Occurrences
  , ixDocTable :: dt
  }

-- ----------------------------------------------------------------------------

newInvertedIndexer :: Indexer InvertedIndex (Hdt.Documents Document)
newInvertedIndexer = Indexer
  { ixIndex    = Ix.empty
  , ixDocTable = Hdt.empty
  }

-- ----------------------------------------------------------------------------

insert :: TextIndexer i dt
       => (Dt.DValue dt) -> Words -> Indexer i dt -> Indexer i dt
insert doc' wrds ix
  = modIndexer newIndex newDocTable ix
  where
  di = ixDocTable ix
  ii = ixIndex    ix
  (did, newDocTable) = Dt.insert di doc'
  newIndex           = addWords wrds did ii

-- | Update elements
update :: TextIndexer i dt => DocId -> Dt.DValue dt -> Words -> Indexer i dt -> Indexer i dt
update docId doc' w ix
  = insert doc' w ix'
  where
  ix' = delete ix (IS.singleton docId)



-- | Modify elements
modify :: (TextIndexer i dt)
       => (Dt.DValue dt -> Dt.DValue dt)
       -> Words -> DocId -> Indexer i dt -> Indexer i dt
modify f wrds dId ix
  = modIndexer newIndex newDocTable ix
  where
  newDocTable = Dt.adjust f dId   (ixDocTable ix)
  newIndex    = addWords wrds dId (ixIndex ix)


-- | Delete a set of documents by 'DocId'
delete :: TextIndexer i dt => Indexer i dt -> DocIdSet -> Indexer i dt
delete i dIds
  = modIndexer newIndex newDocTable i
    where
    newIndex    = Ix.batchDelete dIds $ ixIndex    i
    newDocTable = Dt.difference dIds  $ ixDocTable i

-- | Delete a set if documents by 'URI'.
deleteDocsByURI :: TextIndexer i dt  => Set URI -> Indexer i dt -> Indexer i dt
deleteDocsByURI us ix
      = delete ix docIds
      where
      docIds = toDocIdSet . catMaybesSet . S.map (Dt.lookupByURI (ixDocTable ix)) $ us

-- | Replace the 'Index' .
modIndex    :: TextIndexer i dt => ContextIndex i Occurrences -> Indexer i dt -> Indexer i dt
modIndex = undefined

-- | Replace the  'DocTable'.
modDocTable :: TextIndexer i dt => dt -> Indexer i dt  -> Indexer i dt
modDocTable = undefined

-- | Replace the 'Index' and 'DocTable'.
modIndexer  :: TextIndexer i dt
            => ContextIndex i Occurrences -> dt -> Indexer i dt -> Indexer i dt
modIndexer ii di ix
  = modDocTable di . modIndex ii $ ix

 -- ----------------------------------------------------------------------------

-- | See 'Ix.lookup'.
searchPrefixNoCase :: TextIndexer i dt => Indexer i dt -> Context -> Word -> [(Context,RawResult)]
searchPrefixNoCase ti c w = Ix.lookup PrefixNoCase (Just c,Just w) (ixIndex ti)

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (TextIndexer i dt) =>
                         Description -> Words -> DocId -> Indexer i dt -> Indexer i dt
modifyWithDescription descr wrds dId ix
  = modIndexer newIndex newDocTable ix
  where
  newDocTable = Dt.adjust mergeDescr dId $ ixDocTable ix
  newIndex    = addWords wrds dId $ ixIndex ix
  -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
  mergeDescr  = Doc.update (\d' -> d'{ desc = flip M.union (desc d') descr })
--}
-- ----------------------------------------------------------------------------

-- Helper functions

-- | Add words for a document to the 'Index'.
addWords :: TIx.TextIndex i Occurrences
         => Words -> DocId -> ContextIndex i Occurrences -> ContextIndex i Occurrences
addWords = undefined
--addWords wrds dId i = undefined
{-  = M.foldrWithKey (\c wl acc ->
      M.foldrWithKey (\w ps acc' ->
        Ix.insert (Just c, Just w) (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws
-}

-- Specific to Indexes with Document DocTable values
{-
addDocDescription         :: Description -> DocId -> Indexer it iv i d Document -> Indexer it iv i d Document
addDocDescription descr did (Indexer i d)
  = Indexer i (Dt.modify mergeDescr did d)
  where
  mergeDescr doc = doc{ desc = M.union (desc doc) descr }
-}
----------------------------------------------------------------------------
