{-# LANGUAGE BangPatterns #-}
module Fox.Index.Writer where

import           Fox.Analyze         (Analyzer, runAnalyzer)
import           Fox.Index.Monad
import           Fox.Types

import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict  as IntMap
import           Data.Key
import           Data.Map            (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Set

tryMerge :: IndexWriter ()
tryMerge = undefined

insertDocument :: Document -> IndexWriter Bool
insertDocument doc = insertDocuments [doc]

data Indexer = Indexer !Int !Schema

insertDocuments :: [Document] -> IndexWriter Bool
insertDocuments docs = do
  analyzer <- askAnalyzer
  undefined
  where
{-
  Liste von Dokumenten `docs` :: [Document]

  1: { name: "Alex Metzger", nachname: "Metzger"}
  2: { name: "M Metzger", nachname: "Metzger"}

  docsAndIds = zip [0..] docs




  Map Token (Map Field [DocId])


  DocId -> Document




-}

    invert :: Indexer -> Document -> (Indexer, Document)
    invert (Indexer numberOfDocs schema) doc =
      let
        indexer' = Indexer (numberOfDocs + 1) undefined
        doc'     = undefined
      in (indexer', doc')
