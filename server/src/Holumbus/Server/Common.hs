{-# LANGUAGE FunctionalDependencies #-}
module Holumbus.Server.Common where

import           Control.Monad         (mzero)

import           Data.Aeson
import           Data.Maybe            (fromJust)
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Map              (Map ())
import qualified Data.Map              as M
import           Data.Text             (Text)

import           Holumbus.Index.Common (Context, Word, URI, RawResult, Description, Content
                                       , Position, Occurrences, emptyOccurrences, insertOccurrence
                                       , DocId, Document
                                       , HolIndex, HolDocuments)
import qualified Holumbus.Index.Common as Co

-- | Positions of Words for each context.
type Words        = Map Context WordList

-- | Positions of words in the document.
type WordList     = Map Word [Position]

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri       :: URI
  , apiDocIndexMap  :: Map Context IndexData
  , apiDocDescrMap  :: Description
  }

-- | Data necessary for adding documents to the index.
data IndexData = IndexData
  { idContent       :: Content
  , idMetadata      :: IndexMetadata
  }

-- | Metadata for index processing
data IndexMetadata = IndexMetadata
  { imAnalyzer :: AnalyzerType
  }


-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer

  -- | The default Matadata
defaultIndexMetadata :: IndexMetadata
defaultIndexMetadata = IndexMetadata
  { imAnalyzer = DefaultAnalyzer
  }

-- | empty document
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" M.empty M.empty

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri         <- o    .: "uri"
    indexMap          <- o    .: "index"
    descrMap          <- o    .: "description"
    return ApiDocument
      { apiDocUri       = parsedUri
      , apiDocIndexMap  = indexMap
      , apiDocDescrMap  = descrMap
      }
  parseJSON _ = mzero


instance FromJSON IndexData where
  parseJSON (Object o) = do
    content           <- o    .:  "content"
    metadata          <- o    .:? "metadata" .!= defaultIndexMetadata
    return IndexData
      { idContent       = content
      , idMetadata      = metadata
      }
  parseJSON _ = mzero


instance FromJSON IndexMetadata where
  parseJSON (Object o) = do
    analyzer <- o .: "analyzer" .!= DefaultAnalyzer
    return IndexMetadata
      { imAnalyzer = analyzer
      }
  parseJSON _ = mzero


instance FromJSON AnalyzerType where
  parseJSON (String s) =
    case s of
      "default" -> return DefaultAnalyzer
      _         -> mzero
  parseJSON _ = mzero



instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm) = object
    [ "uri"         .= u
    , "index"       .= im
    , "description" .= dm
    ]

instance ToJSON IndexData where
  toJSON (IndexData c m) = object
    [ "content"     .= c
    , "metadata"    .= m
    ]


instance ToJSON IndexMetadata where
  toJSON (IndexMetadata a) = object
    [ "analyzer"    .= a
    ]

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"


-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure msg) = object
    [ "code"  .= (1 :: Int)
    , "msg"   .= msg
    ]



-- which ops should an indexer support? maybe already in crawler?
-- uses functional dependencies
class (HolIndex i, HolDocuments d) => HolIndexer ix i d | ix -> i d where
  -- insert a new document (and the corresponding words and occurrences) into the indexer
  newIndexer                :: i -> d -> ix
  index                     :: ix -> i
  docTable                  :: ix -> d
  modifyIndexer             :: ix -> i -> d -> ix
  modifyIndex               :: ix -> i -> ix
  modifyIndex ix i          = modifyIndexer ix i (docTable ix)
  modifyDocTable            :: ix -> d -> ix
  modifyDocTable ix         = modifyIndexer ix (index ix)

  -- index functions
  searchPrefixNoCase        :: ix -> Context -> Text -> RawResult
  searchPrefixNoCase        = Co.prefixNoCase . index

  allWords                  :: ix -> Context -> RawResult
  allWords                  = Co.allWords . index

  -- doctable functions
  lookupById                :: Monad m => ix -> DocId -> m Document
  lookupById                = Co.lookupById . docTable

  lookupByURI               :: Monad m => ix -> URI -> m DocId
  lookupByURI               = Co.lookupByURI . docTable

  deleteDocsById            :: Set DocId -> ix -> ix
  deleteDocsById docIds ix  = modifyIndexer ix newIndex newDocTable
    where
      newDocTable = Co.deleteById     docIds (docTable ix)
      newIndex    = Co.deleteDocsById docIds (index    ix)

  deleteDocsByURI           :: Set URI -> ix -> ix
  deleteDocsByURI us ix     = deleteDocsById docIds ix
    where
    docIds = catMaybesSet . S.map (lookupByURI ix) $ us

  updateDoc                 :: DocId -> Document -> Words -> ix -> ix
  updateDoc docId doc w     = insertDoc doc w . deleteDocsById (S.singleton docId)

  insertDoc                 :: Document -> Words -> ix -> ix
  insertDoc doc wrds ix     = modifyIndexer ix newIndex newDocTable
    where
    (dId, newDocTable) = Co.insertDoc (docTable ix) doc
    newIndex           = M.foldrWithKey (\c wl acc -> M.foldrWithKey (\w ps acc' -> Co.insertOccurrences c w (mkOccs dId ps) acc') acc wl) (index ix) wrds

    mkOccs :: DocId -> [Position] -> Occurrences
    mkOccs did pl = insertPositions did pl emptyOccurrences

    insertPositions :: DocId -> [Position] -> Occurrences -> Occurrences
    insertPositions docId ws os = foldr (insertOccurrence docId) os ws


-- generic indexer - combination of an index and a doc table
data Indexer i d
  = Indexer
    { ixIndex    :: i
    , ixDocTable :: d
    }


-- type class for an indexer - combination of index and doctable
instance (HolIndex i, HolDocuments d) => HolIndexer (Indexer i d) i d where
  newIndexer                                    = Indexer
  index               (Indexer i _)             = i
  docTable            (Indexer _ d)             = d
  modifyIndexer       ix i d                    = ix {ixIndex = i, ixDocTable = d}



-- | Data.Maybe.catMaybes on a Set instead of a List.
catMaybesSet :: Ord a => Set (Maybe a) -> Set a
catMaybesSet = S.map fromJust . S.delete Nothing