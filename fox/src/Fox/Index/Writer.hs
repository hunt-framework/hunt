{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fox.Index.Writer where

import           Fox.Index.Directory as IndexDirectory
import           Fox.Index.Monad
import           Fox.Indexer
import           Fox.Schema          (Schema)
import qualified Fox.Schema          as Schema
import           Fox.Types
import           Fox.Types.Document

import           Control.Monad       (when)
import qualified Data.Map.Strict     as Map

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = do
  lookupGlobalFieldTy <- mkLookupGlobalFieldTy
  analyzer            <- askAnalyzer

  let
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

tryMerge :: IndexWriter ()
tryMerge = return ()

flush :: IndexWriter ()
flush = do
  mNewSegment <- createSegment
  case Just mNewSegment of
    Just (sid, segment) -> do
      insertSegment sid segment
      clearIndexer
    Nothing             -> return ()

-- | Create a new @Segment@ from indexed documents. If there are
-- no documents buffered returns @Nothing@.
createSegment :: IndexWriter (SegmentId, Segment)
createSegment = do
  segmentId  <- newSegmentId
  schema     <- indexSchema
  fieldIndex <- bufferedFieldIndex
  documents  <- bufferedDocuments

  let
    fieldOrds = Schema.fieldOrds schema

  withIndexDirectory $ do
    IndexDirectory.writeTermIndex segmentId fieldOrds fieldIndex
    IndexDirectory.writeDocuments segmentId fieldOrds documents

  return (segmentId, Segment firstGeneration)

mkLookupGlobalFieldTy :: IndexWriter LookupGlobalFieldTy
mkLookupGlobalFieldTy = do
  schema <- askSchema
  return $ \fieldName ->
    Schema.internFieldName fieldName schema

maxNumBufferedDocs :: IndexWriter Int
maxNumBufferedDocs = askConfig iwcMaxBufferedDocs

numBufferedDocs :: IndexWriter Int
numBufferedDocs = askIndexer indNumDocs

indexSchema :: IndexWriter Schema
indexSchema = askIndexer indSchema

bufferedFieldIndex :: IndexWriter TermIndex
bufferedFieldIndex = askIndexer indTermIndex

bufferedDocuments :: IndexWriter Documents
bufferedDocuments = askIndexer indDocuments

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
