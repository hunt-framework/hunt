{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies , FlexibleInstances, FlexibleContexts #-}
module TypeclassesFunDeps where

{-
  Added indexer and doctable accessors and mkIndexer function
  to write implementation-independent insert and lookup functions
  Issues:
    - cumbersome implementation delegation
      - mkIndexer does not take all indexer data into account.
        everything except the index and doctable is lost
        - separate modIndex/modDocTable functions?
        - one modify function with Maybes?
          (and default implementation for mod*?)
    - does this work with a distributed indexer?
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

class Index ii iv | ii -> iv where
  insertI :: Word -> iv -> ii -> ii
  lookupI :: Word -> ii -> Result

class DocTable di de | di -> de where
  insertD :: di -> de -> (DocId, di)
  lookupD :: DocId -> di -> Maybe de

class (Index ii iv, DocTable di de) => Indexer ix ii di iv de | ix -> ii di where
  ixIndex    :: ix -> ii
  ixDocTable :: ix -> di
  mkIndexer  :: ii -> di -> ix
  insertIx   :: WordList -> de -> ix -> ix
  lookupIx   :: ix -> Word -> [de]

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

instance Index [(Word, Occs)] Occs where
  insertI = curry L.insert -- no merge
  lookupI =       L.lookup -- only first result

instance DocTable [(DocId, Document)] Document where
  insertD di de = (did, di')
    where
    did = hash de
    di' = L.insert (did, de) di
  lookupD = L.lookup

instance (Index ii Occs, DocTable di de) => Indexer (SimpleIndexer ii di) ii di Occs de where
  mkIndexer   = mkIx
  ixIndex     = siIndex
  ixDocTable  = siDocTable
  insertIx    = insertIx'
  lookupIx    = lookupIx'

-- ----------------------------------------------------------------------------

-- works for any indexer with occs
insertIx' :: (Index ii Occs, DocTable di de, Indexer ix ii di Occs de) =>
          WordList -> de -> ix -> ix
insertIx' wl de ix = mkIndexer ii' di'
    where
    ii         = ixIndex ix
    di         = ixDocTable ix
    (did, di') = insertD di de
    ii' = foldr (\(w, ps) -> insertI w [(did, ps)]) ii wl

lookupIx' :: (Index ii iv, DocTable di de, Indexer ix ii di iv de) =>
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
