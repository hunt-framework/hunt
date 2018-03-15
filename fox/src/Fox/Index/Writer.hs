{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fox.Index.Writer (
    insertDocument
  , insertDocuments

  , doc1
  , doc2
  ) where

import           Fox.Index.Monad
import           Fox.Indexer         as Indexer
import           Fox.Types
import           Fox.Types.Document
import qualified Fox.Index.Directory as Directory
import qualified Fox.Index.InvertedFile as InvertedFile
import qualified Fox.Index.Segment   as Segment
import qualified Fox.Schema          as Schema
import qualified Fox.Types.Conflicts as Conflicts
import qualified Fox.Types.Generation as Generation

import qualified Control.Parallel.Strategies as Parallel
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments []   = return ()
insertDocuments docs = do
  schema          <- askSchema
  analyzer        <- askAnalyzer
  maxBufferedDocs <- maxNumBufferedDocs

  let
    -- we want to intern as many field names as possible
    -- to reduce the number of redundant strings in
    -- memory
    lookupFieldType fieldName =
      Schema.internFieldName fieldName schema

    -- small helper to split a list into chunks as
    -- lazily as possible.
    chunk :: Int -> [a] -> [[a]]
    chunk _ [] =
      []
    chunk n xs =
      as : chunk n bs where (as, bs) = List.splitAt n xs

    -- put docs in batches of size `maxBufferedDocs`
    -- these batches can be indexed in parallel given
    -- we can merge several indexers.
    batches :: [[Document]]
    batches =
      chunk maxBufferedDocs docs

    indexers :: [Either Conflict Indexer]
    indexers = Parallel.parMap Parallel.rpar
                 (Indexer.indexDocs analyzer lookupFieldType) batches

    -- convenience function to avoid bad
    -- indentation by haskell-mode
    foldlM' z xs f = Foldable.foldlM f z xs

  -- Ok, we are indexing all the documents in parallel. We need
  -- to be careful to check for schema compatibility as otherwise
  -- there might  occur inconsistencies.
  schema' <- foldlM' schema indexers $ \prevSchema indexer -> do

    case indexer of
      Right indexed -> do
        let
          conflictingFields :: [(FieldName, FieldType, FieldType)]
          conflictingFields =
            Schema.diffCommonFields (Indexer.indSchema indexed)
                                    prevSchema

        case conflictingFields of
          [] -> do
            -- We know field types are consistent for this indexer.
            -- Store the indexed data into inverted files and note the
            -- new segment.
            (newSegId, segment) <- createSegment indexed
            insertSegment newSegId segment
            return (Schema.union (Indexer.indSchema indexed) prevSchema)

          conflicts ->
            errConflicts [ Conflicts.fieldTyConflict fieldName fieldTy1 fieldTy2
                         | (fieldName, fieldTy1, fieldTy2) <- conflicts
                         ]
      Left conflict -> do
        errConflict conflict

  setSchema schema'

  return ()

-- | Create a new @Segment@ from indexed documents.
createSegment :: Indexer -> IndexWriter (SegmentId, Segment.Segment)
createSegment indexed = do
  segmentId <- newSegmentId
  indexDir  <- askEnv iwIndexDir

  let
    schema =
      indSchema indexed

    termIndex =
      indTermIndex indexed

    fieldOrds =
      Schema.fieldOrds schema

    numberOfDocuments =
      indNumDocs indexed

    segmentDirLayout =
      Directory.segmentDirLayout indexDir segmentId

  runIfM $ do
    InvertedFile.writeInvertedFiles segmentDirLayout
      fieldOrds termIndex

  let
    newSegment =
      Segment.Segment {
          Segment.segGeneration = Generation.genesis
        , Segment.segSchema     = schema
        , Segment.segNumDocs    = numberOfDocuments
      }

  return (segmentId, newSegment)

maxNumBufferedDocs :: IndexWriter Int
maxNumBufferedDocs = askConfig iwcMaxBufferedDocs

doc1 :: Document
doc1 = Document { docWeight = 1.0
                , docFields = HashMap.fromList [
                      ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Moritz Drexl"))
                    , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "rot blau grün"))
                    ]
                }

doc2 :: Document
doc2 = Document { docWeight = 1.0
                , docFields = HashMap.fromList [
                      ("name", DocField (FieldFlags 0x03) 1.0  (FV_Text "Alex Biehl"))
                    , ("lieblingsfarbe", DocField (FieldFlags 0x03) 1.0 (FV_Text "lila rot grün"))
                    ]
                }
