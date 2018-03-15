{-# LANGUAGE FlexibleContexts #-}
module Fox.Indexer where

import           Fox.Analyze                (Analyzer, analyze)
import           Fox.Schema                 (Schema)
import qualified Fox.Schema                 as Schema
import           Fox.Types
import qualified Fox.Types.Conflicts        as Conflicts
import qualified Fox.Types.Document         as Doc
import qualified Fox.Types.Occurrences      as Occurrences

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Key                   (forWithKey_)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq

-- | Try to lookup the type of a field in an environment
-- global to a transaction.
type LookupGlobalFieldTy = FieldName -> Maybe (FieldName, FieldType)

-- | Generate a new @DocId@ by incrementing.
type DocIdGen = DocId

-- | A synonym for an inverted index optimized for
-- insertions of terms.
type TermIndex = Map Term (HashMap FieldName Occurrences)

type Documents = Seq Document

-- | Every @Document@ indexed by an @IndexWrter@ goes into
-- the @Indexer@ first. It collects all kind of useful information
-- and inverts the index which can then be processed into a more
-- efficient form.
data Indexer = Indexer
  { indDocIdGen  :: !DocIdGen
  , indTermIndex :: !TermIndex
  , indDocuments :: !Documents
  , indSchema    :: !Schema
  } deriving (Show)

emptyIndexer :: Indexer
emptyIndexer =
  Indexer { indDocIdGen  = firstDocId
          , indTermIndex = Map.empty
          , indDocuments = Seq.empty
          , indSchema    = Schema.emptySchema
          }

indNumDocs :: Indexer -> Int
indNumDocs ind = Seq.length (indDocuments ind)

-- | Check if an @Indexer@ has no buffered documents.
indNull :: Indexer -> Bool
indNull ind = Map.null (indTermIndex ind)

indexDoc :: Analyzer
         -> Document
         -> LookupGlobalFieldTy
         -> Indexer
         -> Either Conflict Indexer
indexDoc analyzer document lookupGlobalFieldTy indexer = runIndexer $ do
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
      modify $ \s -> s { indDocuments = indDocuments s Seq.|> Doc.filterStorable doc
                       , indDocIdGen  = nextDocId docId
                       }
      pure docId

    fieldTyConflict fieldName fieldTy fieldTy' =
      throwError (Conflicts.fieldTyConflict fieldName fieldTy fieldTy')

    insertFieldTy fieldName fieldTy = do
      schema <- gets indSchema
      case Schema.insertField fieldName fieldTy schema of
        Right (fieldName', schema') -> do
          modify $ \s -> s { indSchema = schema' }
          pure fieldName'
        Left fieldTy' ->
          fieldTyConflict fieldName fieldTy fieldTy'

    -- Makes sure the FieldTypes match up if the FieldName
    -- exists already.
    internFieldTy fieldName fieldValue
      | Just (fieldName', fieldTy') <- lookupGlobalFieldTy fieldName
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
        occs = Occurrences.singleton docId pos

        updateOccs fields =
          HashMap.insertWith
            (\_ -> Occurrences.insert docId pos)
            fieldName
            occs
            fields

        updateField index =
          Map.insertWith
            (\_ -> updateOccs)
            token
            (HashMap.singleton fieldName occs)
            index

      in updateField fieldIndex

    invertField anal fieldName fieldValue docId =
      let
        tokens =
          analyze anal fieldName fieldValue

        invert [] index                       = index
        invert (Token position term:tx) index =
          invert tx $! insertToken fieldName term docId position index
      in do
        modify $ \s -> s { indTermIndex = invert tokens (indTermIndex s) }

indexDocs :: Analyzer
          -> LookupGlobalFieldTy
          -> [Document]
          -> Either Conflict Indexer
indexDocs analyzer getGlobalFieldTy documents =
  foldlM (\indexer' doc ->
            indexDoc analyzer doc getGlobalFieldTy indexer') emptyIndexer documents
