{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Server {-(start)-} where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger
--import           Network.Wai.Middleware.Static

import           Control.Monad            (mzero)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Concurrent.MVar

import           Data.Map                 (Map, insert, empty)
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Aeson hiding        (json)

--import qualified Data.Text.Lazy.Encoding as TEL
--import qualified Data.Text.Lazy as TL


--import           Data.Aeson.Types         --((.:), (.:?), FromJSON, parseJSON, Parser, Value (Array, Object))
--import qualified Data.Aeson as J

import qualified Holumbus.Server.Template       as Tmpl
import           Holumbus.Index.Common hiding   (Attribute)
--import Holumbus.Index.Common.Document
import           Holumbus.Index.Inverted.PrefixMem
--import           Holumbus.Index.Common.RawResult


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
type WordList     = Map Word [Int]
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


-- the index (with random data)
inverted :: Inverted
inverted = fromList emptyInverted [(context, word, occs) | context <- ["context"], word <- ["foo", "foobar"]]
    where
    occs = foldl mergeOccurrences emptyOccurrences
             [ singletonOccurrence (DocId 1) 5
             , singletonOccurrence (DocId 15) 20
             ]


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


-- server itself
start :: IO ()
start = scotty 3000 $ do

  -- index
  invM    <- liftIO $ newMVar inverted
  --(Inverted -> IO b)        -> ActionM b
  let withInv = withIndex' invM :: MonadIO m => (Inverted -> IO b)        -> m b
  --(Inverted -> IO Inverted) -> ActionM ()
  let modInv_ = modIndex_ invM :: MonadIO m => (Inverted -> IO Inverted) -> m ()

  -- request / response logging
  middleware logStdoutDev

  get "/" $ html Tmpl.index

  -- list all indexed documents
  get "/search/:context/:query" $ do
    context <- param "context"
    query   <- param "query"
    res <- withInv $ \i ->
            return . show . resultByWord context $ prefixNoCase i context query
    json $ JsonSuccess res


  -- list all words
  get "/words/:context" $ do
    context <- param "context"
    res <- withInv $ \i ->
            -- simple Text response
            return . show $ allWords i context
    json $ JsonSuccess res

  -- add a document
  post "/document/add" $ do
    -- Raises an exception if parse is unsuccessful
    js <- jsonData
    case js of
      ApiDocument u d _  -> do
        -- transform doc
        let doc = Document u d
        modInv_ $ \i -> do
          -- add to doc table & index
          -- default impl. in HolIndex class for insert uninverted?
          -- the crawler should have some kind of insert implementation
          let modifiedIndex = insertOccurrences "context" "newWord" emptyOccurrences i
          return modifiedIndex
        json (JsonFailure "not implemented" :: JsonResponse Text)

  notFound . redirect $ "/"
