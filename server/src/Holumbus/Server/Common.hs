{-# LANGUAGE FunctionalDependencies #-}
module Holumbus.Server.Common where

import           Control.Monad         (mzero)

import           Data.Aeson
import           Data.Maybe            (fromMaybe, fromJust)
import Data.Set                        (Set)
import qualified Data.Set              as S
import           Data.Map              (Map ())
import qualified Data.Map              as M
import           Data.Text             (Text)

import           Holumbus.Index.Common (Context, Word, URI, Description, RawResult
                                       , Position, Occurrences, emptyOccurrences, insertOccurrence
                                       , DocId, Document
                                       , HolIndex, HolDocuments)
import qualified Holumbus.Index.Common as Co

-- | map from context to a list of words with occurrences
type Words        = Map Context WordList

-- | map from word to a list of occurrences
type WordList     = Map Word [Position]

-- | type the server receives to add/modify/delete? in indexi
-- @FIXME
-- this is way too much work to do for the client implementation
-- we should consider receiving something like an entity with
-- some kind of meta information attached. then our implemented
-- analyzer extracts the words and occurrences to build the index.
--
-- an indexed field would be stored as a context in the index
--
-- may look like this:
--
-- data ApiDocument = ApiDocument URI [Field]
--
-- data Field = Field {
--    name  :: Text
--    value :: Text
--    meta  :: FieldMeta
-- }
--
-- data FieldMeta = FieldMeta {
--    index = IndexType
--    ...
--    ...
-- }
--
-- data IndexType = NO_INDEX | ANALYZER1 | ANALYZER2
--
data ApiDocument = ApiDocument
  { apiDocUri   :: URI
  , apiDocDesc  :: Description
  , apiDocWords :: Words
  } deriving Show

-- | empty document
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" M.empty M.empty

instance ToJSON ApiDocument where
  toJSON (ApiDocument u d ws) = object
    [ "uri"   .= u
    , "desc"  .= toJSON d
    , "words" .= toJSON ws
    ]

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedDesc      <- o    .: "desc"
    parsedUri       <- o    .: "uri"
    parsedWords     <- o    .: "words"
    return ApiDocument
      { apiDocUri     = parsedUri
      , apiDocDesc    = parsedDesc
      , apiDocWords   = parsedWords
      }
  parseJSON _ = mzero


type ApiDocuments = [ApiDocument]

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= toJSON msg
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