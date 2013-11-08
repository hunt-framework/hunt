{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
{-# LANGUAGE NoMonoLocalBinds  #-}

module Holumbus.Server {-(start)-} where

import           Control.Monad.IO.Class               (MonadIO, liftIO)

import           Network.Wai.Middleware.RequestLogger

import           Web.Scotty

import           Data.Text                            (Text)
import qualified Data.Text.Lazy                       as LT

import qualified Data.Aeson                           as A
import           Data.Aeson.Encode.Pretty             (encodePretty)


import           Holumbus.Common
import           Holumbus.Common.ApiDocument
import           Holumbus.Interpreter.Command
import           Holumbus.Interpreter.Interpreter
import           Holumbus.Server.Common
import qualified Holumbus.Server.Template             as Tmpl


-- ----------------------------------------------------------------------------

-- | Like Web'.Scotty.json', but pretty.
jsonPretty :: (A.ToJSON a) => a -> ActionM ()
jsonPretty v = do
  setHeader "Content-Type" "application/json"
  raw $ encodePretty v

-- ----------------------------------------------------------------------------

-- server itself:
--
--  -> should get some kind of state from command line or config file
--     f.e: which index impl to use, which doc store, which persistent backend etc...

start :: IO ()
start = scotty 3000 $ do

  -- interpreter
  env <- liftIO $ initEnv emptyIndexer emptyOptions
  let interpret = runCmd env

  let eval cmd = do
      res <- liftIO $ interpret cmd
      case res of
        Left  (ResError code msg) ->
          json $ (JsonFailure code [msg] :: JsonResponse Text)
        Right res' ->
          case res' of
            ResOK               -> json $ JsonSuccess ("ok" :: Text)
            ResSearch docs      -> json $ JsonSuccess docs
            ResCompletion wrds  -> json $ JsonSuccess wrds

  -- request / response logging
  middleware logStdoutDev

  get "/"         $ redirect "/search"
  get "/search"   $ html . Tmpl.index $ (0::Int)
  get "/add"      $ html Tmpl.addDocs

  -- interpreter
  post "/eval" $ do
    -- Raises an exception if parse is unsuccessful
    cmd <- jsonData :: ActionM Command
    eval cmd

  -- simple query
  get "/search/:query/" $ do
    query    <- param "query"
    -- XXX: mix of lazy and strict Text
    eval (Search (Left . LT.toStrict $ query) 1 1000000)

  -- paged query
  get "/search/:query/:page/:perPage" $ do
    query    <- param "query"
    p        <- param "page"
    pp       <- param "perPage"
    -- XXX: mix of lazy and strict Text
    eval (Search (Left . LT.toStrict $ query) p pp)

  -- completion
  get "/completion/:query" $ do
    query <- param "query"
    -- XXX: mix of lazy and strict Text
    eval (Completion . LT.toStrict $ query)

  -- insert a document (fails if a document (the uri) already exists)
  post "/document/insert" $ do
    -- Raises an exception if parse is unsuccessful
    jss <- jsonData :: ActionM [ApiDocument]
    let batch = Sequence $ map (flip Insert New) jss
    eval batch

  -- delete a set of documents by URI
  post "/document/delete" $ do
    jss <- jsonData :: ActionM [URI]
    let batch = Sequence $ map Delete jss
    eval batch

  -- write the indexer to disk
  get "/binary/save/:filename" $ do
    filename  <- param "filename"
    eval $ LoadIx filename

  -- load indexer from disk
  get "/binary/load/:filename" $ do
    filename  <- param "filename"
    eval $ StoreIx filename

  notFound $ text "page not found"
