{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fox.Index.Writer where

import           Fox.Analyze                (Analyzer, analyze)
import           Fox.Index.Directory        as IndexDirectory
import           Fox.Index.Monad
import           Fox.Schema                 (Schema)
import qualified Fox.Schema                 as Schema
import           Fox.Types
import           Fox.Types.Document
import qualified Fox.Types.Document         as Doc
import qualified Fox.Types.Occurrences      as Occurrences

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Key                   (forWithKey_)
import qualified Data.Map.Strict            as Map
import qualified Data.Sequence              as Seq

-- | Try to lookup the type of a field in an environment
-- global to a transaction.
type GlobalFieldTy = FieldName -> Maybe (FieldName, FieldType)

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = do
  analyzer        <- askAnalyzer
  schema          <- askSchema
  let
    lookupGlobalFieldTy fieldName =
      (\ty -> (fieldName, ty)) <$> Schema.lookupField fieldName schema

    -- put docs in batches of size `maxBufferedDocs`
    -- these batches can be indexed in parallel given
    -- we can merge several indexers.
    _batches =
      [docs]

  withIndexer $ indexDocs analyzer docs lookupGlobalFieldTy

  -- Lots of documents have been indexed
  -- already. Flush buffer to disk to make
  -- room for new documents.
  maxBufferedDocs <- maxNumBufferedDocs
  bufferedDocs    <- numBufferedDocs
  let
    freeBufferSpace =
      min (maxBufferedDocs - bufferedDocs) 0
  when (freeBufferSpace <= 0) flush

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
  forWithKey_ (docFields document) $ \fieldName docField -> do
    -- note the type of field even if field is not indexable
    -- we want to know all fields occurring in the docs.
    fieldName' <- internFieldTy fieldName (dfValue docField)
    when (Doc.fieldIndexable (dfFlags docField)) $ do
      invertField analyzer fieldName' (dfValue docField) docId
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

    fieldTyConflict fieldName fieldTy fieldTy' =
      throwError $ ConflictFields fieldName fieldTy fieldTy'

    insertFieldTy fieldName fieldTy = do
      schema <- indSchema <$> get
      case Schema.insertField fieldName fieldTy schema of
        Right (fieldName', schema') -> do
          modify $ \s -> s { indSchema = schema' }
          pure fieldName'
        Left fieldTy' ->
          fieldTyConflict fieldName fieldTy fieldTy'

    -- Makes sure the FieldTypes match up if the FieldName
    -- exists already.
    internFieldTy fieldName fieldValue
      | Just (fieldName', fieldTy') <- getGlobalFieldTy fieldName
      = if fieldTy == fieldTy'
        then insertFieldTy fieldName' fieldTy
        else fieldTyConflict fieldName fieldTy fieldTy'

      | otherwise
      = insertFieldTy fieldName fieldTy
      where
        fieldTy = fieldType fieldValue

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
        tokens = analyze anal fieldName fieldValue

        invert [] index                       = index
        invert (Token position term:tx) index =
          invert tx $! insertToken fieldName term docId position index
      in do
        modify $ \s -> s { indIndex = invert tokens (indIndex s) }

tryMerge :: IndexWriter ()
tryMerge = return ()

flush :: IndexWriter ()
flush = do
  mNewSegment <- createSegment
  case mNewSegment of
    Just (sid, segment) -> do
      insertSegment sid segment
      clearIndexer
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
      documents  <- bufferedDocuments

      let
        -- fieldOrd is total over any field the indexer
        -- saw.
        fieldOrd :: FieldName -> Int
        fieldOrd =
          Schema.lookupFieldOrd (Schema.fieldOrds schema) 

      withIndexDirectory $ do
        IndexDirectory.writeTermIndex segmentId fieldOrd fieldIndex
        IndexDirectory.writeDocuments segmentId undefined fieldOrd documents

      return (segmentId, Segment firstGeneration)

maxNumBufferedDocs :: IndexWriter Int
maxNumBufferedDocs = askConfig iwcMaxBufferedDocs

numBufferedDocs :: IndexWriter Int
numBufferedDocs = askIndexer indNumDocs

indexSchema :: IndexWriter Schema
indexSchema = Schema.uninternSchema <$> askIndexer indSchema

bufferedFieldIndex :: IndexWriter FieldIndex
bufferedFieldIndex = askIndexer indIndex

bufferedDocuments :: IndexWriter BufferedDocs
bufferedDocuments = askIndexer indDocs

clearIndexer :: IndexWriter ()
clearIndexer = withIndexer (\_ -> pure emptyIndexer)

doc1 :: Document
doc1 = Document { docWeight = 1.0
                , docFields = Map.fromList [
                      ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Moritz Drexl"))
                    , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "rot blau grün"))
                    ]
                }

doc2 :: Document
doc2 = Document { docWeight = 1.0
                , docFields = Map.fromList [
                      ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Alex Biehl"))
                    , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "lila rot grün"))
                    ]
                }
