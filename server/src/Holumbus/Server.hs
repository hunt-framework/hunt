{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
{-# LANGUAGE NoMonoLocalBinds  #-}

module Holumbus.Server {-(start)-} where

import           Prelude                              as P

import           Control.Monad.IO.Class               (MonadIO, liftIO)

import           Network.Wai.Middleware.RequestLogger

import           Web.Scotty

--import qualified Data.Binary                          as Bin
--import           Data.Either                          (partitionEithers)
--import qualified Data.Map                             as M
import qualified Data.Set                             as S
import           Data.Text                            (Text)
import qualified Data.Text.Lazy                       as LT

import qualified Data.Aeson                           as A
import           Data.Aeson.Encode.Pretty             (encodePretty)

import           Holumbus.Common

--import qualified Holumbus.Index.Proxy.CachedIndex       as IxCache

--import qualified Holumbus.Index.Text.Inverted.PrefixMem as Inv
--import           Holumbus.DocTable.DocTable           hiding (filter, map)
--import qualified Holumbus.DocTable.DocTable           as Dt
--import qualified Holumbus.DocTable.HashedDocuments    as Dt
--import           Holumbus.Index.InvertedIndex         as Inv
--import           Holumbus.Index.TextIndex             (TextIndex)
--import qualified Holumbus.Index.TextIndex             as TIx
--import qualified Holumbus.Indexer.TextIndexer         as Ixx

--import           Holumbus.Server.Analyzer
import           Holumbus.Server.Common
import qualified Holumbus.Server.Template             as Tmpl
--import qualified Holumbus.Server.Interpreter          as Ip
import           Holumbus.Interpreter.Command
import           Holumbus.Interpreter.Interpreter

import           Holumbus.Common.ApiDocument

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
  
  let interpret' cmd = do
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
  get "/search"   $ do
    html . Tmpl.index $ (0::Int)
  get "/add"      $ html Tmpl.addDocs

  -- interpreter
  post "/document/interpret" $ do
    -- Raises an exception if parse is unsuccessful
    cmd <- jsonData :: ActionM Command
    interpret' cmd

  -- simple query
  get "/search/:query" $ do
    query    <- param "query"
    -- XXX: mix of lazy and strict Text
    interpret' (Search . Left . LT.toStrict $ query)

  -- completion
  get "/completion/:query" $ do
    query <- param "query"
    -- XXX: mix of lazy and strict Text
    interpret' (Completion . LT.toStrict $ query)

  -- insert a document (fails if a document (the uri) already exists)
  post "/document/insert" $ do
    -- Raises an exception if parse is unsuccessful
    jss <- jsonData :: ActionM [ApiDocument]
    let batch = Sequence $ foldr (\doc cmds -> (Insert doc New):cmds) [] jss
    interpret' batch

  -- delete a set of documents by URI
  post "/document/delete" $ do
    uris <- jsonData :: ActionM (S.Set URI)
    interpret' $ Delete uris
  
  -- write the indexer to disk
  get "/binary/save/:filename" $ do
    filename  <- param "filename"
    interpret' $ LoadIx filename

  -- load indexer from disk
  get "/binary/load/:filename" $ do
    filename  <- param "filename"
    interpret' $ StoreIx filename

  notFound $ text "page not found"

-- ----------------------------------------------------------------------------

{-
  -- simple query with paging
  get "/search/:query/:page/:perPage" $ do
    query    <- param "query"
    p        <- param "page"
    pp       <- param "perPage"
    res      <- runQuery query
    case res of
      (JsonSuccess docs) -> json . JsonSuccess $ mkPagedResult docs p pp
      _                  -> json res
-}
    -- res :: Maybe [URI]  -- maybe the documents that already exist
{--    res <- modIx $ \ix -> do
      -- intersection of new docs and docTable
      let uris = checkApiDocUrisAbsence jss ix :: Either [(URI, DocId)] [URI]
      -- empty intersection of sets -> safe to insert
      return $ either
        (\existingUris ->
            (ix, return . map fst $ existingUris))
        (\_            ->
             ( foldr (uncurry Ixx.insert . toDocAndWords') ix jss
            , Nothing))
        uris
    json $ maybe
            (JsonSuccess "document(s) added" :: JsonResponse Text)
            JsonFailure
            res
--}
{--
  -- updated/replaces a document (fails if a document (the uri) does not exists)
  post "/document/replace" $ do
    -- Raises an exception if parse is unsuccessful
    jss <- jsonData :: ActionM [ApiDocument]

    -- res :: Maybe [URI]  -- maybe the documents/uris that do not exist
    res <- modIx $ \ix -> do
      let uris = checkApiDocUrisExistence jss ix :: Either [URI] [(URI, DocId)]
      -- set of new docs minus docTable
      -- difference set is empty -> safe to update
      return $ either
        (\nonexistentUris ->
            (ix, return nonexistentUris))
        (\existentDocs    ->
            ( foldr (\(docId, apiDoc) ->
                      uncurry (Ixx.update docId) . toDocAndWords' $ apiDoc)
                    ix
                    (flip zip jss . map snd $ existentDocs)
            , Nothing))
        uris

    json $ maybe
            (JsonSuccess "document(s) replaced" :: JsonResponse Text)
            JsonFailure
            res


  -- update/modify a document (fails if a document (the uri) does not exists)
  post "/document/update" $ do
    -- Raises an exception if parse is unsuccessful
    jss <- jsonData :: ActionM [ApiDocument]

    -- res :: Maybe [URI]  -- maybe the documents/uris that do not exist
    res <- modIx $ \ix -> do
      let uris = checkApiDocUrisExistence jss ix :: Either [URI] [(URI, DocId)]
      -- set of new docs minus docTable
      -- difference set is empty -> safe to update
      return $ either
        (\nonexistentUris ->
            (ix, return nonexistentUris))
        (\existentDocs    ->
            ( foldr
                (\(docId, apiDoc) ->
                  Ixx.modifyWithDescription
                  (apiDocDescrMap apiDoc)
                  (snd . toDocAndWords' $ apiDoc)
                  docId)
                ix
                (flip zip jss . map snd $ existentDocs)
            , Nothing))
        uris

    json $ maybe
            (JsonSuccess "document description(s) updated" :: JsonResponse Text)
            JsonFailure
            res

  -- TODO: proper load/save, routes, get/post/.., filenames, exception handling etc.

  let mkIndexerPath = (++ ".ixx")
--}
-- ----------------------------------------------------------------------------

{--
-- Functions to check the existence/absence of documents (by URI) in the indexer.

-- | Constructs a list of 'ApiDocument' 'URI's that already exist in the indexer and a
--   ist of 'URI's and corresponding 'DocId's for 'ApiDocument's which are part of the indexer.
checkApiDocUris :: [ApiDocument] -> Indexer Inv.InvertedIndex (Dt.Documents Document) -> ([URI], [(URI, DocId)])
checkApiDocUris apiDocs ix =
  let apiDocsE
          = map (\apiDoc -> let docUri = apiDocUri apiDoc
                                idM    = Dt.lookupByURI (Ixx.ixDocTable ix) docUri
                            in maybe
                                (          Left   docUri)
                                (\docId -> Right (docUri, docId))
                                idM)
                apiDocs
  in partitionEithers apiDocsE

-- | Constructs a Right value using the second accessor function
--   if the first accessor function is an empty list. Otherwise the non-empty list is returned.
--   Used for 'checkApiDocUrisExistence' and 'checkApiDocUrisAbsence'
checkApiDocUris' :: (([URI], [(URI, DocId)]) -> [a], ([URI], [(URI, DocId)]) -> b)
                    -> [ApiDocument] ->  Indexer Inv.InvertedIndex (Dt.Documents Document)  -> Either [a] b
checkApiDocUris' (l,r) apiDocs ix =
    let res = checkApiDocUris apiDocs ix
    in (if P.null . l $ res then Right . r else Left . l) res

-- | Checks whether the Documents with the URIs supplied by ApiDocuments are in the index,
--   i.e. before an insert operation.
--   Left is the failure case where there are URIs given that cannot be found in the index.
checkApiDocUrisExistence :: [ApiDocument] -> Indexer Inv.InvertedIndex (Dt.Documents Document) -> Either [URI] [(URI, DocId)]
checkApiDocUrisExistence = checkApiDocUris' (fst, snd)

-- XXX: maybe return Either [URI] [URI], since the DocIds will not be needed in the failure case
-- | Checks whether there are no Documents with the URIs supplied by ApiDocuments in the index,
--   i.e. before an update operation.
--   Left is the failure case where there are already documents with those URIs in the index.
checkApiDocUrisAbsence   :: [ApiDocument] -> Indexer Inv.InvertedIndex (Dt.Documents Document) -> Either [(URI, DocId)] [URI]
checkApiDocUrisAbsence   = checkApiDocUris' (snd, fst)

-- | This is just used to specify the exact type of @e@ in @('DocumentWrapper' e)@
toDocAndWords' :: (DocumentWrapper e, e ~ CompressedDoc) => ApiDocument -> (e, Words)
toDocAndWords' = toDocAndWords
--}
