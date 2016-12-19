{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Fox.Index.Writer where

import           Fox.Analyze         (Analyzer, runAnalyzer)
import           Fox.Index.Monad
import           Fox.Indexer
import           Fox.Types

import Fox.Types.Document
import Fox.Types.DocDesc

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict  as IntMap
import           Data.Key
import           Data.Map            (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Set
import           Data.Traversable

tryMerge :: IndexWriter ()
tryMerge = undefined

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = do
  analyzer <- askAnalyzer
  schema   <- askFullSchema

  let
    lookupGlobalFieldTy fieldName =
      HashMap.lookup fieldName schema

    indexResult = runIndexer lookupGlobalFieldTy $
      for docs $ \doc -> invertDocument analyzer doc

  case indexResult of
    Right (docIds, indexed) -> do
      return ()
    Left conflict -> errConflict conflict

doc1 = Document { docWeight = 1.0
                , docFields = [ ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Moritz Drexl"))
                              , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "rot blau grün"))
                              ]
                }

doc2 = Document { docWeight = 1.0
                , docFields = [ ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Alex Biehl"))
                              , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "lila rot grün"))
                              ]
                }




{-
  Liste von Dokumenten `docs` :: [Document]

  1: { name: "Alex Metzger", nachname: "Metzger"}
  2: { name: "M Metzger", nachname: "Metzger"}

  Alex -> "name"
  Metger -> {("name", [(1, 2),(2, 2)]), ("nachname", 1)}

  docsAndIds = zip [0..] docs

  Map Token (Map Field [DocId])

  DocId -> Document
-}
