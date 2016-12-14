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

type DocId = Integer

insertDocuments :: [Document] -> IndexWriter Bool
insertDocuments docs = do
  analyzer <- askAnalyzer
  undefined
  where
{-
  Liste von Dokumenten `docs` :: [Document]

  1: { name: "Alex Metzger", nachname: "Metzger"}
  2: { name: "M Metzger", nachname: "Metzger"}

  docsAndIds = map zip [0..] docFields

  type Result = Map Token (Map FieldName [DocId])
  start [Document] -> [(DocId, (FieldName, DocField))]
  start = zip [0..] . map docFields
  

  analyze :: DocId -> FieldName -> DocField -> [(Token, FieldName, FieldType, DocId)]
  analyze = undefined

  checkForConflict :: [(Token, FieldName, FieldType, DocId)] -> Either Conflict [(Token, FieldName, DocId)]

  insert :: Token -> FieldName -> DocId -> Result -> Result
  insert = undefined


  (Token, [(FieldName, [DocId])])

  Map Token (Map FieldName [DocId])


  DocId -> Document




-}

    invert :: Indexer -> Document -> (Indexer, Document)
    invert (Indexer numberOfDocs schema) doc =
      let
        indexer' = Indexer (numberOfDocs + 1) undefined
        doc'     = undefined
      in (indexer', doc')
