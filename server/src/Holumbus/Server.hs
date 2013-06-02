{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Holumbus.Server {-(start)-} where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger
--import           Network.Wai.Middleware.Static

import           Control.Monad            (mzero)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Concurrent.MVar

import           Data.Map                 (Map, empty)
import qualified Data.Map                 as M
import           Data.Text                (Text)
--import qualified Data.Text                as T
import           Data.Aeson hiding        (json)

--import qualified Data.Text.Lazy.Encoding as TEL
--import qualified Data.Text.Lazy as TL


--import           Data.Aeson.Types         --((.:), (.:?), FromJSON, parseJSON, Parser, Value (Array, Object))
--import qualified Data.Aeson as J

import qualified Holumbus.Server.Template       as Tmpl
import           Holumbus.Index.Common          (Position, Word, Context, URI, Description, Document(..), RawResult, DocId(..)
                                                , HolIndex, HolDocuments)
import qualified Holumbus.Index.Common          as Co
import           Holumbus.Index.Common.Occurences
--import Holumbus.Index.Common.Document
import           Holumbus.Index.Inverted.PrefixMem
--import           Holumbus.Index.Common.RawResult
import           Holumbus.Index.CompactDocuments


--
-- incoming documents:
--
-- Description contains data for persistent storage of document
-- Words contains data to for index structures
--
type Attribute    = Text
--type Description  = Map Attribute T.Text
type Words        = Map Context WordList
--type Context      = Text
type WordList     = Map Word [Position]
--type Word         = Text

data ApiDocument = ApiDocument
  { apiDocUri     :: URI
  , apiDocDesc    :: Description
  , apiDocWords   :: Words
  } deriving Show

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" empty empty

-- some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure Text

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= toJSON msg
    ]

  toJSON (JsonFailure msg) = object
    [ "code"  .= (1 :: Int)
    , "msg"   .= msg
    ]

-- document to outgoing json result

instance ToJSON ApiDocument where
  toJSON (ApiDocument u d ws) = object
    [ "uri"   .= u
    , "desc"  .= toJSON d
    , "words" .= toJSON ws
    ]

-- incoming json to apidocument
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


-- which ops should an indexer support? maybe already in crawler?
-- uses functional dependencies
class (HolIndex i, HolDocuments d) => HolIndexer ix i d | ix -> i d, i d -> ix where
  -- insert a new document (and the corresponding words and occurrences) into the indexer
  newIndexer                :: i -> d -> ix
  index                     :: ix -> i
  docTable                  :: ix -> d
  insertDoc                 :: Document -> Words -> ix -> ix
  searchPrefixNoCase        :: ix -> Context -> String -> RawResult
  allWords                  :: ix -> Context -> RawResult


-- generic indexer - combination of an index and a doc table
data Indexer i d
  = Indexer
    { ixIndex    :: i
    , ixDocTable :: d
    }


-- type class for an indexer - combination of index and doctable
instance (HolIndex i, HolDocuments d) => HolIndexer (Indexer i d) i d where
  newIndexer          i d                       = Indexer i d
  index               (Indexer i _)             = i
  docTable            (Indexer _ d)             = d
  searchPrefixNoCase                            = Co.prefixNoCase . index
  allWords                                      = Co.allWords . index
  -- FIXME: insert the doc and words as into the index and the document table
  insertDoc      doc wrds ix                    = newIndexer newIndex newDocTable
    where
    (dId, newDocTable) = Co.insertDoc (docTable ix) doc
    -- insertDoc                     :: d -> Document -> (DocId, d)
    newIndex           = foldr (\(c, w, ps) -> Co.insertOccurrences c w (mkOccs dId ps)) (index ix) $ flattenWords wrds

    mkOccs :: DocId -> [Position] -> Occurrences
    mkOccs did pl = insertOccs did pl emptyOccurrences

    insertOccs :: DocId -> [Position] -> Occurrences -> Occurrences
    insertOccs docId ws os = foldr (insertOccurrence docId) os ws
    
    flattenWords :: Map t (Map t1 t2) -> [(t, t1, t2)]
    flattenWords = concat . map (\(c, wl) -> map (\(w, ps)-> (c, w, ps)) $ M.toList wl) . M.toList


(.::) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.::) = (.).(.)

-- do something with the index
withIndex' :: MonadIO m => MVar a -> (a -> IO b) -> m b
withIndex' im a = liftIO $ readMVar im >>= a

-- modify the index
modIndex_ :: MonadIO m => MVar a -> (a -> IO a) -> m ()
modIndex_ = liftIO .:: modifyMVar_

-- modify the index with return value
modIndex :: MonadIO m => MVar a -> (a -> IO (a,b)) -> m b
modIndex = liftIO .:: modifyMVar


-- the index (with random data)
inverted :: Inverted
inverted = Co.fromList emptyInverted [(context, word, occs) | context <- ["context"], word <- ["foo", "foobar"]]
    where
    occs = foldl Co.mergeOccurrences Co.emptyOccurrences
             [ Co.singletonOccurrence (DocId 1) 5
             , Co.singletonOccurrence (DocId 15) 20
             ]


indexer :: Indexer Inverted Documents
indexer = Indexer emptyInverted emptyDocuments


-- server itself
start :: IO ()
start = scotty 3000 $ do

  -- index
  ixM    <- liftIO $ newMVar indexer
  let withIx = withIndex' ixM :: MonadIO m => (Indexer Inverted Documents -> IO b) -> m b
  let modIx_ = modIndex_ ixM :: MonadIO m => (Indexer Inverted Documents -> IO (Indexer Inverted Documents)) -> m ()

  -- request / response logging
  middleware logStdoutDev

  get "/" $ html Tmpl.index

  -- list all indexed documents
  get "/search/:context/:query" $ do
    context <- param "context"
    query   <- param "query"
    res <- withIx $ \i ->
            return . show . Co.resultByWord context $ searchPrefixNoCase i context query
    json $ JsonSuccess res


  -- list all words
  get "/words/:context" $ do
    context <- param "context"
    res <- withIx $ \i ->
            -- simple Text response
            return . show $ allWords i context
    json $ JsonSuccess res

  -- add a document
  post "/document/add" $ do
    -- Raises an exception if parse is unsuccessful
    js <- jsonData
    case js of
      ApiDocument u d ws  -> do
        -- transform doc
        let doc = Document u d
        modIx_ $ \ix ->
          -- insertDocument is not implemented yet
          return $ insertDoc doc ws ix
        json (JsonSuccess "doc added" :: JsonResponse Text)

  notFound . redirect $ "/"
