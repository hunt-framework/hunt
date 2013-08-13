{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Server {-(start)-} where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

import qualified Data.Map                                 as M
import           Data.Maybe                               (fromJust, isJust,
                                                           isNothing)
import qualified Data.Set                                 as S
import           Data.Text                                (Text)
{-
import qualified Data.Aeson                               as A
import           Data.Aeson.Encode.Pretty                 (encodePretty)
-}
import           Holumbus.Utility                         ((.::))

import           Holumbus.Index.Common
import qualified Holumbus.Index.Common.DocIdMap           as DM

import           Holumbus.DocTable.DocTable               hiding (filter, map)
import           Holumbus.Index.TextIndex

import           Holumbus.DocTable.HashedCompactDocuments as HCD
--import           Holumbus.DocTable.HashedDocuments        as HD
import           Holumbus.Index.Text.Inverted.PrefixMem

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Result

import           Holumbus.Indexer.TextIndexer             as Ix
import           Holumbus.Server.Analyzer
import           Holumbus.Server.Common
import qualified Holumbus.Server.Template                 as Tmpl




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
--indexer :: Indexer Inverted HD.Documents Document
--indexer = Indexer emptyIndex HD.emptyDocTable

indexer :: TextIndexer Inverted (DocTable HCD.Documents HCD.CompressedDoc) Document
indexer = Indexer emptyIndex HCD.emptyDocTable

queryConfig :: ProcessConfig
queryConfig = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM :: Monad m => TextIndex i -> DocTable d Document -> Query -> m Result
runQueryM i d q = processQueryM queryConfig i d q

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

checkApiDocUris :: (Monad m, Functor m)
                => (m DocId -> Bool) -> [ApiDocument] -> Indexer it v i d de -> [(URI, m DocId)]
checkApiDocUris filterDocIds apiDocs ix =
  let apiDocsM
          = map (\apiDoc -> let docUri = apiDocUri apiDoc
                            in (docUri, Ix.lookupByURI ix docUri)) apiDocs
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
  let modIx_ = modIndex_  ixM -- :: MonadIO m => (Indexer Inverted Documents -> IO (Indexer Inverted Documents))    -> m ()
  let modIx  = modIndex   ixM -- :: MonadIO m => (Indexer Inverted Documents -> IO (Indexer Inverted Documents, b)) -> m b

  -- request / response logging
  middleware logStdoutDev


  get "/" $ html Tmpl.index
  get "/add" $ html Tmpl.addDocs


  -- text "should get simple text query as param"
  -- Note: route /search not handled here!
  get "/search/:query" $ do
    queryStr <- param "query"
    res      <- withIx $ \ix -> do
                          case parseQuery queryStr of
                            (Left err) -> return . JsonFailure . return $ err
                            (Right query) ->
                              runQueryM (ixIndex ix) (ixDocTable ix) query
                              >>= return . JsonSuccess . map (\(_,(DocInfo doc _,_)) -> doc) . DM.toList . docHits
    json res


  get "/completion/:query" $ do
    queryStr <- param "query"
    res      <- withIx $ \ix -> do
                          case parseQuery queryStr of
                            (Left err) -> return . JsonFailure . return $ err
                            (Right query) ->
                              runQueryM (ixIndex ix) (ixDocTable ix) query
                              >>= return . JsonSuccess . map (\ (c, (_, o)) -> (c, M.foldr (\m r -> r + DM.size m) 0 o)) . M.toList. wordHits
    json res


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
          ( foldr (uncurry Ix.insertDoc . toDocAndWords) ix jss
          , Nothing)
       else (ix, return failedDocUris)

    json $ maybe
            (JsonSuccess "document(s) added" :: JsonResponse Text)
            JsonFailure
            res


  -- updated/replaces a document (fails if a document (the uri) does not exists)
  post "/document/replace" $ do
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
          ( foldr (\(docId, apiDoc) ->
                      uncurry (Ix.updateDoc docId) . toDocAndWords $ apiDoc) ix (zip (map (fromJust . snd) allDocs) jss)
          , Nothing)
       else (ix, return failedDocUris)

    json $ maybe
            (JsonSuccess "document(s) updated" :: JsonResponse Text)
            JsonFailure
            res


  -- update/modify a document (fails if a document (the uri) does not exists)
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
          ( foldr (\(docId, apiDoc) ->
                      Ix.modifyWithDescription (apiDocDescrMap apiDoc) (snd . toDocAndWords $ apiDoc) docId) ix (zip (map (fromJust . snd) allDocs) jss)
          , Nothing)
       else (ix, return failedDocUris)

    json $ maybe
            (JsonSuccess "document description(s) updated" :: JsonResponse Text)
            JsonFailure
            res


  -- delete a set of documents by URI
  post "/document/delete" $ do
    jss <- jsonData :: ActionM (S.Set URI)
    modIx_ $ \ix -> return $ deleteDocsByURI jss ix
    json (JsonSuccess "document(s) deleted" :: JsonResponse Text)


  notFound $ text "page not found"
