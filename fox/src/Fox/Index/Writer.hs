{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fox.Index.Writer where

import           Fox.Analyze                (Analyzer, runAnalyzer)
import           Fox.Index.Monad
import           Fox.Types
import           Fox.Types.Document
import qualified Fox.Types.Document         as Doc
import qualified Fox.Types.Occurrences      as Occurrences

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map.Strict            as Map
import qualified Data.Sequence              as Seq

-- | Try to lookup the type of a field in an environment
-- global to a transaction.
type GlobalFieldTy = FieldName -> Maybe FieldType

tryMerge :: IndexWriter ()
tryMerge = undefined

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = do
  analyzer        <- askAnalyzer
  schema          <- askSchema
  bufferedDocs    <- numBufferedDocs
  maxBufferedDocs <- maxBufferedDocs

  let
    lookupGlobalFieldTy fieldName =
      HashMap.lookup fieldName schema

  withIndexer $ indexDocs analyzer docs lookupGlobalFieldTy

maxBufferedDocs :: IndexWriter Int
maxBufferedDocs = askConfig iwcMaxBufferedDocs

numBufferedDocs :: IndexWriter Int
numBufferedDocs = askIndexer indNumDocs

indexDocs :: Analyzer
          -> [Document]
          -> GlobalFieldTy
          -> Indexer
          -> Either Conflict Indexer
indexDocs analyzer documents getGlobalFieldTy indexer =
  foldlM (\indexer' doc ->
            indexDoc analyzer doc getGlobalFieldTy indexer') indexer documents

indexDoc :: Analyzer
         -> Document
         -> GlobalFieldTy
         -> Indexer
         -> Either Conflict Indexer
indexDoc analyzer document getGlobalFieldTy indexer = runIndexer $ do
  docId <- insertDoc document
  for_ (docFields document) $ \(fieldName, docField) -> do
    -- note the type of field even if field is not indexable
    -- we want to know all fields occurring in the docs.
    checkFieldTy fieldName (dfValue docField)
    when (Doc.fieldIndexable (dfFlags docField)) $ do
      invertField analyzer fieldName (dfValue docField) docId
  where
    -- we are only interested in the Indexer, not
    -- in the result.
    runIndexer m = runExcept (execStateT m indexer)

    -- insert the document into the buffer and return
    -- its DocId for further reference.
    insertDoc doc = do
      docId <- gets indDocIdGen
      modify $ \s -> s { indDocs     = indDocs s Seq.|> Doc.filterStorable doc
                       , indDocIdGen = nextDocId docId
                       }
      return docId

    -- ask for type of FieldName through 1. and 2.
    -- 1. Ask for the type in a Schema which is global to this transaction.
    -- 2. If not found in 1. look in transaction local Schema
    getFieldTy fieldName = do
      schema <- gets indSchema
      return $ getGlobalFieldTy fieldName
        <|> HashMap.lookup fieldName schema

    putFieldTy fieldName fieldTy = do
      modify $ \s -> s { indSchema =
                           HashMap.insert fieldName fieldTy (indSchema s) }

    fieldTyConflict fieldName fieldTy fieldTy' =
      throwError $ ConflictFields fieldName fieldTy fieldTy'

    -- Makes sure the FieldTypes match up if the FieldName
    -- exists already.
    checkFieldTy fieldName fieldValue = do
      let fieldTy' = fieldType fieldValue
      mFieldTy <- getFieldTy fieldName
      putFieldTy fieldName fieldTy'
      case mFieldTy of
        Just fieldTy | fieldTy /= fieldTy' ->
                         fieldTyConflict fieldName fieldTy fieldTy'
        _ -> return ()

    -- this is the meat: create a mapping: Token -> FieldName -> Occurrences
    insertToken fieldName token docId pos fieldIndex =
      let
        singleton = Occurrences.singleton docId pos

        updateOccs fields = Map.insertWith
                            (\_ occs -> Occurrences.insert docId pos occs)
                            fieldName
                            singleton
                            fields

        updateField index = Map.insertWith
                            (\_ fields -> updateOccs fields)
                            token
                            (Map.singleton fieldName singleton)
                            index
      in updateField fieldIndex

    invertField anal fieldName fieldValue docId =
      let
        tokens = runAnalyzer anal fieldName fieldValue

        invert !_position [] index          = index
        invert !position (token:toks) index =
          invert (position + 1) toks $! insertToken fieldName token docId position index
      in do
        modify $ \s -> s { indIndex = invert 0 tokens (indIndex s) }

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
