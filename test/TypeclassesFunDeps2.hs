{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies , FlexibleInstances, FlexibleContexts #-}
module TypeclassesFunDeps where

{-
  An experiment using a structure which resembles the current structure more closely.
  Issues:
    - insertIx/lookupIx should not be implementation dependent
    - adding an additional indexer type parameter causes ambiguity
      (like the 'elem' type in the current implementation)
      - maybe a bad idea anyway
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
  insertIx :: WordList -> de -> ix -> ix
  lookupIx :: ix -> Word -> [de]

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
  insertIx wl de (SimpleIndexer ii di) = mkIx ii' di'
    where
    (did, di') = insertD di de
    ii' = foldr (\(w, ps) -> insertI w [(did, ps)]) ii wl
  lookupIx (SimpleIndexer ii di) w = maybe [] (mapMaybe (flip lookupD di . fst)) $ lookupI w ii

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
search = lookupIx ixx
