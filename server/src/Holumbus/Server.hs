{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Server {-(start)-} where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger
--import           Network.Wai.Middleware.Static

--import           Control.Monad            (mzero)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Concurrent.MVar

import           Data.Maybe               (isJust, isNothing, fromJust)
import qualified Data.Set as              S
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
{-
import qualified Data.Aeson               as A
import           Data.Aeson.Encode.Pretty (encodePretty)
-}
--import qualified Data.Text.Lazy.Encoding as TEL
--import qualified Data.Text.Lazy as TL

--import           Data.Aeson.Types         --((.:), (.:?), FromJSON, parseJSON, Parser, Value (Array, Object))
--import qualified Data.Aeson as J

import qualified Holumbus.Server.Template       as Tmpl
import           Holumbus.Server.Common

import           Holumbus.Index.Common          ( URI
                                                , Document(..)
                                                , DocId(..)
                                                , HolIndex, HolDocuments)
import qualified Holumbus.Index.Common          as Co
import           Holumbus.Index.Inverted.PrefixMem
import           Holumbus.Index.HashedDocuments

import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Result



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

-- the indexer
indexer :: Indexer Inverted Documents
indexer = newIndexer emptyInverted emptyDocuments

queryConfig :: ProcessConfig
queryConfig = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQuery :: (HolIndex i, HolDocuments d) => i -> d -> Query -> Result
runQuery = processQuery queryConfig

-- Replacement for the scotty json function for pretty JSON encoding.
-- There should be a new release of scotty soon (end of the month?)
-- which will most likely contain the 'raw' function which is introduced
-- with the yet to be merged pull request:
-- https://github.com/xich/scotty/pull/33
{-
jsonPretty :: (A.ToJSON a) => a -> ActionM ()
jsonPretty v = do
  header "Content-Type" "application/json"
  raw $ encodePretty v
-}

checkApiDocUris :: HolIndexer ix i d => (Maybe DocId -> Bool) -> [ApiDocument] -> ix -> [(URI, Maybe DocId)]
checkApiDocUris filterDocIds apiDocs ix =
  let apiDocsM
          = map (\apiDoc -> let docUri = apiDocUri apiDoc
                            in (docUri, lookupByURI ix docUri)) apiDocs
  in filter (filterDocIds . snd) apiDocsM


-- server itself:
--
--  -> should get some kind of state from command line or config file
--     f.e: which index impl to use, which doc store, which persistent backend etc...
start :: IO ()
start = scotty 3000 $ do

  -- index
  ixM    <- liftIO $ newMVar indexer
  let withIx = withIndex' ixM -- :: MonadIO m => (Indexer Inverted Documents -> IO b) -> m b
  let modIx_ = modIndex_  ixM -- :: MonadIO m => (Indexer Inverted Documents -> IO (Indexer Inverted Documents)) -> m ()
  let modIx  = modIndex   ixM

  -- request / response logging
  middleware logStdoutDev


  get "/" $ html Tmpl.index


  -- text "should get simple text query as param"
  -- Note: route /search not handled here!
  get "/search/:query" $ do
    queryStr <- param "query"
    res      <- withIx $ \ix -> do
                          case parseQuery queryStr of
                            (Left err) -> return . JsonFailure . return $ err
                            (Right query) -> return . JsonSuccess
                              $ map (\(_,(DocInfo doc _,_)) -> doc)
                              $ Co.toListDocIdMap . docHits
                              $ runQuery (index ix) (docTable ix) query
    json res


  get "/completion/:query" $ do
    queryStr <- param "query"
    res      <- withIx $ \ix -> do
                          case parseQuery queryStr of
                            (Left err) -> return . JsonFailure . return $ err
                            (Right query) -> return . JsonSuccess
                              $ map (\ (c, (_, o)) -> (c, M.fold (\m r -> r + Co.sizeDocIdMap m) 0 o))
                              $ M.toList. wordHits
                              $ runQuery (index ix) (docTable ix) query
    json res


  -- list all words
  get "/words/:context" $ do
    context <- param "context"
    res <- withIx $ \i ->
            -- simple Text response
            return . show $ allWords i $ T.pack context
    json $ JsonSuccess res


  -- insert a document (fails if a document (the uri) already exists)
  post "/document/insert" $ do
    -- Raises an exception if parse is unsuccessful
    jss <- jsonData :: ActionM [ApiDocument]

    -- res :: Maybe [URI]  -- maybe the documents that already exist
    res <- modIx $ \ix -> do
      -- intersection of new docs and docTable
      let failedDocUris = map fst $ checkApiDocUris isJust jss ix :: [URI]
      -- empty intersection of sets -> safe to insert
      return $ if Prelude.null failedDocUris
       then
          ( foldr (\(ApiDocument u d ws) ->
                      let doc = Document u d
                      in insertDoc doc ws) ix jss
          , Nothing)
       else (ix, return failedDocUris)

    json $ maybe
            (          JsonSuccess "document(s) added" :: JsonResponse Text)
            (\errL ->  JsonFailure errL)
            res


-- updated/replaces a document (fails if a document (the uri) does not exists)
  post "/document/update" $ do
    -- Raises an exception if parse is unsuccessful
    jss <- jsonData :: ActionM [ApiDocument]

    -- res :: Maybe [URI]  -- maybe the documents/uris that do not exist
    res <- modIx $ \ix -> do
      let allDocs       = checkApiDocUris (const True) jss ix :: [(URI, Maybe DocId)]
      -- set of new docs minus docTable
      let failedDocUris = map fst . filter (isNothing . snd) $ allDocs
      -- difference set is empty -> safe to update
      return $ if Prelude.null failedDocUris
       then
          ( foldr (\(docId, ApiDocument u d ws) ->
                      let doc = Document u d
                      in updateDoc docId doc ws) ix (zip (map (fromJust . snd) allDocs) jss)
          , Nothing)
       else (ix, return failedDocUris)

    json $ maybe
            (          JsonSuccess "document(s) updated" :: JsonResponse Text)
            (\errL ->  JsonFailure errL)
            res


  get "/document/delete/:uri" $ do
    docUri <- param "uri"
    modIx_ $ \ix -> return $ deleteDocsByURI (S.singleton . T.pack $ docUri) ix
    json (JsonSuccess "document deleted" :: JsonResponse Text)


  notFound $ text "page not found"
