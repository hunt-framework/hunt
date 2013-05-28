{-# LANGUAGE OverloadedStrings #-}
module Holumbus.Server {-(start)-} where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger

import           Control.Applicative
import           Control.Monad            (mzero)

import           Data.Map
import qualified Data.Text as T
--import qualified Text.Blaze.Html5 as H
--import Text.Blaze.Html.Renderer.Text (renderHtml)
--import Text.Blaze.Html5 ((!))
--import qualified Text.Blaze.Html5.Attributes as A
import           Data.Monoid              (mconcat)
import           Data.Aeson hiding        (json)
--import           Data.Aeson.Types         --((.:), (.:?), FromJSON, parseJSON, Parser, Value (Array, Object))
--import qualified Data.Aeson as J


-- import holu 1.3.2 types
--import Holumbus.Index.Common

--
-- incoming documents:
--
-- Description contains data for persistent storage of document
-- Words contains data to for index structures
--
data ApiDocument = ApiDocument
  { apiDocUri   :: Uri
  , apiDocDesc  :: Description
  , apiDocWords :: Words
  } deriving Show
type Attribute = String
type Description = Map Attribute String
type Words = Map Context WordList
type Context = String
type WordList = Map Word [Int]
type Word = String
type Uri = String
--
-- outgoing documents
--
-- search results contain serialized documents:
--
data Document = Document (Uri, Description)
--
--
data Customer = Customer T.Text T.Text Int


-- we dont really need this...
instance ToJSON ApiDocument where
  toJSON (ApiDocument uri desc contextWords) = object
    [ "uri"   .= uri
    , "desc"  .= toJSON desc
    , "words" .= toJSON contextWords
    ]


-- we need this - but ... how???
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



instance FromJSON Customer where
  parseJSON (Object v)
    = Customer
        <$> v .: "name"
        <*> v .: "address"
        <*> v .: "age"
  parseJSON _ = mzero

-- server itself
start :: IO ()
start = scotty 3000 $ do
  let documents = [ ApiDocument "1"
                      (Data.Map.fromList [("title", "document1"), ("content", "... ... ... ")])
                      (Data.Map.fromList [("context1", Data.Map.fromList [("hallo", [2,6,7])])])
                  ]

  middleware logStdoutDev

  get "/" $ text "mainpage"

 -- get "/customers" $ json customers

  get "/documents/:index" $ do
    index <- param "index"
    if length documents > index
      then json $ documents !! index
      else text $ "index out of range"

  get "/:word" $ do
    txt <- param "word"
    html $ mconcat ["<h1>", txt, "</h1>"]

  notFound . redirect $ "/"
