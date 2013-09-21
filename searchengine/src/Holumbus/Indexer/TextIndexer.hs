{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Holumbus.Indexer.TextIndexer
  ( module Holumbus.Indexer.Indexer
  , TextIndexer
  , searchPrefixNoCase
  , allWords
  , modifyWithDescription
  , update
  , insert
  , modify
  )
where

import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M

import           Holumbus.Index.Common
import qualified Holumbus.Index.Common.Document    as Doc
import qualified Holumbus.Index.Common.Occurrences as Occ

import qualified Holumbus.DocTable.DocTable        as Dt

import qualified Holumbus.Index.Index              as Ix
import           Holumbus.Index.TextIndex          (TextIndex)

import           Holumbus.Indexer.Indexer          hiding (insert, modify,
                                                    update)
import qualified Holumbus.Indexer.Indexer          as Ixx

-- ----------------------------------------------------------------------------

-- TODO: remove Words constraint
-- | TextIndexer with 'Index' implementation, 'DocTable' implementation and element type parameter.
--   Uses 'Textual' and 'Occurrences' as index and value type.
--   Uses 'Words' as elem type.
type TextIndexer i = (Indexer i, TextIndex (IxIndex i), IxElem i ~ Words)

-- ----------------------------------------------------------------------------

-- index functions

-- TODO: lonely text function?
-- | See 'Ix.lookup'.
searchPrefixNoCase    :: TextIndexer i => i -> Context -> Word -> RawResult
searchPrefixNoCase
  = Ix.lookup PrefixNoCase . ixIndex

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
