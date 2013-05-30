{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Holumbus.Server {-(start)-} where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger
--import           Network.Wai.Middleware.Static

import           Control.Monad            (mzero)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Concurrent.MVar

import           Data.Map hiding ((!))
import           Data.Text                (Text)
import qualified Data.Text as T
import           Data.Aeson hiding        (json)

import           Text.Blaze.Html.Renderer.Text (renderHtml)
--import qualified Data.Text.Lazy.Encoding as TEL
--import qualified Data.Text.Lazy as TL


--import           Data.Aeson.Types         --((.:), (.:?), FromJSON, parseJSON, Parser, Value (Array, Object))
--import qualified Data.Aeson as J

import qualified Holumbus.Server.Template as Tmpl


--
-- incoming documents:
--
-- Description contains data for persistent storage of document
-- Words contains data to for index structures
--
type Attribute    = Text
type Description  = Map Attribute T.Text
type Words        = Map Context WordList
type Context      = Text
type WordList     = Map Word [Int]
type Word         = Text
type Uri          = Text

data ApiDocument = ApiDocument
  { apiDocUri     :: Uri
  , apiDocDesc    :: Description
  , apiDocWords   :: Words
  } deriving Show

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument
  { apiDocUri     = "id::1"
  , apiDocDesc    = (insert "title" "empty document" $ empty)
  , apiDocWords   = (insert "defaultContext" (insert "word" [] $ empty) $ empty)
  }

-- outgoing documents
--
-- search results contain serialized documents
-- this document should be imported from searchengine later
--
data Document     = Document
  { docUri  :: Uri
  , docDesc :: Description
  }
  deriving Show

emptyDoc :: Document
emptyDoc = Document
  { docUri     = "id::1"
  , docDesc    = (insert "title" "empty document" $ empty)
  }

-- some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure String

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
instance ToJSON Document where
  toJSON (Document uri desc) = object
    [ "uri"   .= uri
    , "desc"  .= toJSON desc
    ]

instance ToJSON ApiDocument where
  toJSON (ApiDocument uri desc ws) = object
    [ "uri"   .= uri
    , "desc"  .= toJSON desc
    , "words" .= toJSON ws
    ]

-- incoming json to apidocument
instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    desc      <- o    .: "desc"
    uri       <- o    .: "uri"
    conWords  <- o    .: "words"
    return ApiDocument
      { apiDocUri     = uri
      , apiDocDesc    = desc
      , apiDocWords   = conWords
      }
  parseJSON _ = mzero


-- server itself
start :: IO ()
start = scotty 3000 $ do

  -- tmp documents store
  docs    <- liftIO $ newMVar [emptyDoc]

  -- request / response logging
  middleware logStdoutDev

  get "/" $ html $ Tmpl.index

  -- list all indexed documents
  get "/search/:query" $ do
    ds <-liftIO (readMVar docs)
    json $ JsonSuccess ds

  -- get a specific document
  get "/document/:index" $ do
    index <- param "index"
    ds <- liftIO $ readMVar docs
    if length ds > index
      then json $ ds !! index
      else text $ "index out of range"

  -- add a document
  post "/document/add" $ do
    -- Raises an exception if parse is unsuccessful
    js <- jsonData
    case js of
      ApiDocument u d _  -> do
        liftIO $ modifyMVar_ docs (\ds -> return $ (Document u d):ds)
        json $ JsonSuccess ("document added"::T.Text)

  notFound . redirect $ "/"
