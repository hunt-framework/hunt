{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module TypeclassesAssocTypes where

{-
  Direct translation of TypeclassesFunDeps3.hs
  using associated types.
-}

import qualified Data.List as L
import           Data.Char
import           Data.Maybe

-- ----------------------------------------------------------------------------

-- type aliases

type DocId      = Int
type Word       = String
type Document   = String
type Result     = Maybe Occs
type Occs       = [(DocId, Positions)]
type WordList   = [(Word, Positions)]
type Positions  = [Int]

-- ----------------------------------------------------------------------------

-- index, doctable and indexer type classes

class Index i where
  type IVal i :: *
  insertI :: Word -> IVal i -> i -> i
  lookupI :: Word -> i -> Result

class DocTable i where
  type DVal i :: *
  insertD :: i -> DVal i -> (DocId, i)
  lookupD :: DocId -> i -> Maybe (DVal i)

class (Index (IxIndex i), DocTable (IxDocTable i)) =>
      Indexer i where
  type IxIndex    i :: *
  type IxDocTable i :: *
  ixIndex    :: i -> IxIndex i
  ixDocTable :: i -> IxDocTable i
  mkIndexer  :: IxIndex i -> IxDocTable i -> i
  insertIx   :: WordList -> DVal (IxDocTable i) -> i -> i
  lookupIx   :: i -> Word -> [DVal (IxDocTable i)]

-- ----------------------------------------------------------------------------

-- a simple indexer

data SimpleIndexer ii di = SimpleIndexer
  { siIndex     :: ii
  , siDocTable  :: di
  }

mkIx :: ii -> di -> SimpleIndexer ii di
mkIx = SimpleIndexer

-- ----------------------------------------------------------------------------

-- the implementation has a few issues, but that is not really the point,
-- it serves a type-check purpose

instance Index [(Word, Occs)] where
  type IVal [(Word, Occs)] = Occs
  insertI = curry L.insert -- no merge
  lookupI =       L.lookup -- only first result

instance DocTable [(DocId, Document)] where
  type DVal [(DocId, Document)] = Document
  insertD di de = (did, di')
    where
    did = hash de
    di' = L.insert (did, de) di
  lookupD = L.lookup

instance (Index ii, DocTable di, IVal ii ~ Occs) =>
         Indexer (SimpleIndexer ii di) where
  type IxIndex    (SimpleIndexer ii di) = ii
  type IxDocTable (SimpleIndexer ii di) = di

  mkIndexer   = mkIx
  ixIndex     = siIndex
  ixDocTable  = siDocTable
  insertIx    = insertIx'
  lookupIx    = lookupIx'

-- ----------------------------------------------------------------------------

-- works for any indexer with occs
insertIx' :: (Indexer ix, de ~ DVal (IxDocTable ix), IVal (IxIndex ix) ~ Occs) =>
             WordList -> de -> ix -> ix
insertIx' wl de ix = mkIndexer ii' di'
    where
    ii         = ixIndex ix
    di         = ixDocTable ix
    (did, di') = insertD di de
    ii' = foldr (\(w, ps) -> insertI w [(did, ps)]) ii wl

lookupIx' :: (Indexer ix, de ~ DVal (IxDocTable ix)) =>
             ix -> Word -> [de]
lookupIx' ix w = maybe [] (mapMaybe (flip lookupD di . fst)) $ lookupI w ii
  where
  ii = ixIndex ix
  di = ixDocTable ix

-- ----------------------------------------------------------------------------

hash :: Document -> DocId
hash = sum . map ord

-- ----------------------------------------------------------------------------

ixx :: SimpleIndexer [(Word, Occs)] [(DocId, Document)]
ixx = mkIx [] []

docs :: [(WordList, Document)]
docs = [ ([("word1", [])], "doc1")
       , ([("word2", [])], "doc2")]

ws :: [(Word, Positions)]
ws = [("word",[1])]

ixx' :: SimpleIndexer [(Word, Occs)] [(DocId, Document)]
ixx' = foldr (uncurry insertIx) ixx docs

search :: Word -> [Document]
search = lookupIx ixx'
