{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
{-# LANGUAGE NoMonoLocalBinds  #-}

module Holumbus.Server {-(start)-} where

import           Control.Monad.Error

import           Network.Wai.Middleware.RequestLogger

import           Data.Text                            (Text)

import           Holumbus.Common

import           Holumbus.Interpreter.Command
import           Holumbus.Interpreter.Interpreter

import           Holumbus.Query.Language.Parser

import           Holumbus.Server.Common
import qualified Holumbus.Server.Template             as Tmpl
import           Holumbus.Server.Schrotty

-- ----------------------------------------------------------------------------

-- server itself:
--
--  -> should get some kind of state from command line or config file
--     f.e: which index impl to use, which doc store, which persistent backend etc...

start :: IO ()
start = do
  -- init interpreter
  env <- initEnv emptyIndexer emptyOptions

  -- start schrotty
  schrotty 3000 $ do

  -- request / response logging
  middleware logStdoutDev

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


  get "/"         $ redirect "/search"
  get "/search"   $ html . Tmpl.index $ (0::Int)
  get "/add"      $ html Tmpl.addDocs

  -- interpreter
  post "/eval" $ do
    cmd <- jsonData
    eval cmd

  -- simple query
  get "/search/:query/" $ do
    query    <- param "query"
    case parseQuery query of
      Right qry -> eval (Search qry 0 1000000)
      Left  err -> json $ (JsonFailure 700 [err] :: JsonResponse Text)

  -- paged query
  get "/search/:query/:offset/:mx" $ do
    query    <- param "query"
    offset   <- param "offset"
    mx       <- param "mx"
    case parseQuery query of
      Right qry -> eval (Search qry offset mx)
      Left  err -> json $ (JsonFailure 700 [err] :: JsonResponse Text)

  -- completion
  get "/completion/:query/:mx" $ do
    query <- param "query"
    mx    <- param "mx"
    case parseQuery query of
      Right qry -> eval (Completion qry mx)
      Left  err -> json $ (JsonFailure 700 [err] :: JsonResponse Text)

  -- insert a document (fails if a document (the uri) already exists)
  post "/document/insert" $ do
    jss <- jsonData
    let batch = Sequence $ map Insert jss
    eval batch

  -- update a document (fails if a document (the uri) does not exist)
  post "/document/update" $ do
    jss <- jsonData
    let batch = Sequence $ map Update jss
    eval batch

  -- delete a set of documents by URI
  post "/document/delete" $ do
    jss <- jsonData
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

  notFound $ throwWE $ NotFound
    -- req <- request
    -- let path = BSL.fromStrict $ rawPathInfo req
