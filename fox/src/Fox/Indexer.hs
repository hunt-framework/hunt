{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fox.Indexer where

import           Fox.Analyze
import           Fox.Index.Monad            (Conflict (..))
import           Fox.Types
import qualified Fox.Types.Occurrences      as Occurrences
import qualified Fox.Types.Document         as Doc

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable
import qualified Data.HashMap.Strict        as HashMap
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as Map
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq

type DocIdGen = DocId

type FieldIndex = Map Token (Map FieldName Occurrences)

data IndexerState =
  IndexerState { isGlobalFieldTy :: FieldName -> Maybe FieldType
               , isSchema        :: !Schema
               , isDocIdGen      :: !DocIdGen
               , isInverted      :: !FieldIndex
               , isDocs          :: !(Seq Document)
               }

newtype Indexer a =
  Indexer { unIndexer :: StateT IndexerState (Except Conflict) a }
  deriving (Functor, Applicative, Monad)

runIndexer :: (FieldName -> Maybe FieldType)
           -> Indexer a
           -> Either Conflict (a, IndexerState)
runIndexer luType indexer =
  let
    state = IndexerState { isGlobalFieldTy = luType
                         , isSchema        = HashMap.empty
                         , isDocIdGen      = firstDocId
                         , isInverted      = Map.empty
                         , isDocs          = Seq.empty
                         }
  in runExcept (runStateT (unIndexer indexer) state)
  
insertDoc :: Document -> Indexer DocId
insertDoc doc = Indexer $ do
  docId <- gets isDocIdGen
  modify $ \s -> s { isDocs     = isDocs s Seq.|> Doc.filterStorable doc
                   , isDocIdGen = nextDocId docId
                   }
  return docId
  
-- | Given a @FieldName@ we need to lookup its @FieldType@
-- (if present). We first check in the global @Schema@
-- and fallback to transaction local @Schema@.
getFieldTy :: FieldName -> Indexer (Maybe FieldType)
getFieldTy fieldName = Indexer $ do
  getGlobalFieldTy <- gets isGlobalFieldTy
  schema           <- gets isSchema
  return $ getGlobalFieldTy fieldName
    <|> HashMap.lookup fieldName schema

putFieldTy :: FieldName -> FieldType -> Indexer ()
putFieldTy fieldName fieldTy = Indexer $ do
  modify $ \s -> s { isSchema =
      HashMap.insert fieldName fieldTy (isSchema s) }

-- | Raises @ConflictFields@ error on diverging @FieldType@s.
fieldTyConflict :: FieldName -> FieldType -> FieldType -> Indexer a
fieldTyConflict fieldName fieldTy fieldTy' =
  Indexer $ throwError (ConflictFields fieldName fieldTy fieldTy')

checkFieldTy :: FieldName -> FieldValue -> Indexer FieldType
checkFieldTy fieldName fieldValue = do
  let fieldTy' = fieldType fieldValue
  mFieldTy <- getFieldTy fieldName
  case mFieldTy of
    Just fieldTy | fieldTy == fieldTy' -> return fieldTy
                 | otherwise -> fieldTyConflict fieldName fieldTy fieldTy'
    Nothing -> do putFieldTy fieldName fieldTy'
                  return fieldTy'

insertToken :: FieldName -> Token -> DocId -> Int -> FieldIndex -> FieldIndex
insertToken fieldName token docId pos fieldIndex =
  let
    singOccs = Occurrences.singleton docId pos

    insertField fields = Map.insertWith
                         (\_ occs -> Occurrences.insert docId pos occs)
                         fieldName singOccs fields

    insert index = Map.insertWith
                   (\_ fields -> insertField fields)
                   token
                   (Map.singleton fieldName singOccs)
                   index
  in
    insert fieldIndex

invertField :: FieldName -> [Token] -> DocId -> Indexer ()
invertField fieldName tokens docId = Indexer $
  modify $ \s -> s { isInverted = invert 0 tokens (isInverted s) }
  where
    invert !_position []          fix = fix
    invert !position (token:toks) fix =
      invert (position + 1) toks $! insertToken fieldName token docId position fix

invertDocument :: Analyzer -> Document -> Indexer DocId
invertDocument anal doc = do
  docId <- insertDoc doc
  for_ (docFields doc) $ \(fieldName, docField) -> do
    -- note the type of field even if field is not indexable
    fieldTy <- checkFieldTy fieldName (dfValue docField)
    when (Doc.fieldIndexable (dfFlags docField)) $ do 
      let tokens = runAnalyzer anal fieldName (dfValue docField)
      invertField fieldName tokens docId    
  return docId
