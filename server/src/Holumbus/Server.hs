{-# LANGUAGE OverloadedStrings #-}
module Holumbus.Server (start) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Control.Applicative
import Control.Monad (mzero)

import qualified Data.Text as T
--import qualified Text.Blaze.Html5 as H
--import Text.Blaze.Html.Renderer.Text (renderHtml)
--import Text.Blaze.Html5 ((!))
--import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mconcat)
import Data.Aeson hiding (json)
import qualified Data.Aeson as J

-- import holu 1.3.2 types
import Holumbus.Index.Common 


-- entity to test json 
data Customer = Customer {
  name :: T.Text ,
  address :: T.Text,
  age :: Int
}

-- from/to json instances
instance ToJSON Customer where
  toJSON (Customer n a age) = object 
    [ "name" .= n
    , "address" .= a
    , "age" .= age
    ]

instance FromJSON Customer where
  parseJSON (Object v) = Customer 
                      <$> v .: "name"
                      <*> v .: "address"
                      <*> v .: "age"
  parseJSON _ = mzero

-- server itself
start = scotty 3000 $ do
  let customers = [ Customer "Chris" "Musterstraße 34" 28
                  , Customer "Tom" "Dorfstraße 23" 30
                  ] 

  middleware logStdoutDev

  get "/" $ text "mainpage"

  get "/customers" $ json customers

  get "/customer/:index" $ do
    index <- param "index"
    if length customers > index
      then json $ customers !! index
      else text $ "index out of range"
 
  get "/:word" $ do
    txt <- param "word"
    html $ mconcat ["<h1>", txt, "</h1>"]

  notFound . redirect $ "/"
