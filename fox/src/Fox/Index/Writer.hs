{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fox.Index.Writer where

import           Fox.Analyze                (Analyzer, runAnalyzer)
import           Fox.Index.Monad
import Fox.Index.Directory as IndexDirectory
import           Fox.Types
import           Fox.Types.Document
import qualified Fox.Types.Document         as Doc
import qualified Fox.Types.Occurrences      as Occurrences

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable
import qualified Data.HashMap.Strict        as HashMap
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Sequence              as Seq

-- | Try to lookup the type of a field in an environment
-- global to a transaction.
type GlobalFieldTy = FieldName -> Maybe FieldType

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = do
  analyzer        <- askAnalyzer
  schema          <- askSchema
  bufferedDocs    <- numBufferedDocs
  maxBufferedDocs <- maxNumBufferedDocs

  let
    lookupGlobalFieldTy fieldName =
      HashMap.lookup fieldName schema

    freeBufferSpace =
      min (maxBufferedDocs - bufferedDocs) 0

    -- put docs in batches of size `maxBufferedDocs`
    -- these batches can be indexed in parallel given
    -- we can merge several indexers.
    _batches =
      [docs]


  -- Lots of documents have been indexed
  -- already. Flush buffer to disk to make
  -- room for new documents.
  when (freeBufferSpace <= 0) flush

  withIndexer $ indexDocs analyzer docs lookupGlobalFieldTy

maxNumBufferedDocs :: IndexWriter Int
maxNumBufferedDocs = askConfig iwcMaxBufferedDocs

numBufferedDocs :: IndexWriter Int
numBufferedDocs = askIndexer indNumDocs

indexSchema :: IndexWriter Schema
indexSchema = askIndexer indSchema

bufferedFieldIndex :: IndexWriter FieldIndex
bufferedFieldIndex = askIndexer indIndex

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

tryMerge :: IndexWriter ()
tryMerge = return ()

flush :: IndexWriter ()
flush = do
  mNewSegment <- createSegment
  case mNewSegment of
    Just (sid, segment) -> insertSegment sid segment
    Nothing             -> return ()

-- | Create a new @Segment@ from indexed documents. If there are
-- no documents buffered returns @Nothing@.
createSegment :: IndexWriter (Maybe (SegmentId, Segment))
createSegment = do
  isEmpty <- askIndexer indNull
  if not isEmpty
    then Just <$> createSegment_
    else return Nothing
  where
    createSegment_ = do
      segmentId  <- newSegmentId
      schema     <- indexSchema
      fieldIndex <- bufferedFieldIndex

      let
        -- we sort every field known to the indexer to
        -- use stable numbers as shorter field names.
        sortedFields :: Map FieldName FieldType
        sortedFields = Map.fromList (HashMap.toList schema)

        -- fieldOrd is total over any field the indexer
        -- saw.
        fieldOrd :: FieldName -> Int
        fieldOrd fieldName =
          Map.findIndex fieldName sortedFields

      withIndexDirectory $ \indexDirectory -> do
        IndexDirectory.writeTermIndex indexDirectory segmentId fieldOrd fieldIndex

      return (segmentId, undefined)

doc1 :: Document
doc1 = Document { docWeight = 1.0
                , docFields = [ ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Moritz Drexl"))
                              , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "rot blau grün"))
                              ]
                }

doc2 :: Document
doc2 = Document { docWeight = 1.0
                , docFields = [ ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Alex Biehl"))
                              , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "lila rot grün"))
                              ]
                }
