{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Server {-(start)-} where

import           Prelude                                  as P

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class                   (MonadIO, liftIO)

import           Network.Wai.Middleware.RequestLogger

import           Web.Scotty

import           Data.Either                              (partitionEithers)
import qualified Data.Map                                 as M
import qualified Data.Set                                 as S
import           Data.Text                                (Text)
{-
import qualified Data.Aeson                               as A
import           Data.Aeson.Encode.Pretty                 (encodePretty)
-}
import           Holumbus.Utility                         ((.::))

import           Holumbus.Index.Common
import qualified Holumbus.Index.Common.DocIdMap           as DM

--import           Holumbus.Index.Common.Document           as Doc
import           Holumbus.Index.Common.CompressedDocument as Doc

import qualified Holumbus.Index.Proxy.CachedIndex         as IxCache
import           Holumbus.Index.Text.Inverted.PrefixMem
import qualified Holumbus.Index.Text.Inverted.PrefixMem   as Inv
import           Holumbus.Index.TextIndex

import           Holumbus.DocTable.DocTable               hiding (filter, map)
import qualified Holumbus.DocTable.DocTable               as Dtt
import           Holumbus.DocTable.HashedDocuments        as Dt

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Result

import           Holumbus.Indexer.Indexer                 as Ix
import           Holumbus.Indexer.TextIndexer             (TextIndexer, modifyWithDescription,
                                                           newTextIndexer)
import           Holumbus.Server.Analyzer
import           Holumbus.Server.Common
import qualified Holumbus.Server.Template                 as Tmpl

-- ----------------------------------------------------------------------------

-- do something with the index
withIndex'      :: MonadIO m => MVar a -> (a -> IO b) -> m b
withIndex' im a = liftIO $ readMVar im >>= a

-- modify the index
modIndex_       :: MonadIO m => MVar a -> (a -> IO a) -> m ()
modIndex_       = liftIO .:: modifyMVar_

-- modify the index with return value
modIndex        :: MonadIO m => MVar a -> (a -> IO (a,b)) -> m b
modIndex        = liftIO .:: modifyMVar

-- the indexer
--indexer :: Indexer Inverted HD.Documents Document
--indexer = Indexer emptyIndex HD.emptyDocTable

indexer         :: TextIndexer Inverted Documents DocumentWrapper
indexer         = newTextIndexer (IxCache.empty Inv.empty) Dt.empty

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: Monad m => TextIndex i -> DocTable d DocumentWrapper -> Query -> m Result
runQueryM       = processQueryM queryConfig

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

-- Functions to check the existence/absence of documents (by URI) in the indexer.

-- | Constructs a list of 'ApiDocument' 'URI's that already exist in the indexer and a
--   ist of 'URI's and corresponding 'DocId's for 'ApiDocument's which are part of the indexer.
checkApiDocUris :: [ApiDocument] -> Indexer elem it v i d de -> ([URI], [(URI, DocId)])
checkApiDocUris apiDocs ix =
  let apiDocsE
          = map (\apiDoc -> let docUri = apiDocUri apiDoc
                                idM    = Ix.lookupByURI ix docUri
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
                    -> [ApiDocument] -> Indexer elem it v i d de -> Either [a] b
checkApiDocUris' (l,r) apiDocs ix =
    let res = checkApiDocUris apiDocs ix
    in if P.null . l $ res then Right . r $ res else Left . l $ res

-- | Checks whether the Documents with the URIs supplied by ApiDocuments are in the index,
--   i.e. before an insert operation.
--   Left is the failure case where there are URIs given that cannot be found in the index.
checkApiDocUrisExistence :: [ApiDocument] -> Indexer elem it v i d de -> Either [URI] [(URI, DocId)]
checkApiDocUrisExistence = checkApiDocUris' (fst, snd)

-- XXX: maybe return Either [URI] [URI], since the DocIds will not be needed in the failure case
-- | Checks whether there are no Documents with the URIs supplied by ApiDocuments in the index,
--   i.e. before an update operation.
--   Left is the failure case where there are already documents with those URIs in the index.
checkApiDocUrisAbsence   :: [ApiDocument] -> Indexer elem it v i d de -> Either [(URI, DocId)] [URI]
checkApiDocUrisAbsence   = checkApiDocUris' (snd, fst)


toDocAndWords' :: ApiDocument -> (DocumentWrapper, Words)
toDocAndWords' = toDocAndWords Doc.wrapDoc

-- ----------------------------------------------------------------------------

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
  let runQuery = \queryStr -> withIx $ \ix -> do
      case parseQuery queryStr of
        (Left err) -> return . JsonFailure . return $ err
        (Right query) ->
          runQueryM (ixIndex ix) (ixDocTable ix) query
          >>= return . JsonSuccess . map (\(_,(DocInfo d _,_)) -> doc d) . DM.toList . docHits

  -- request / response logging
  middleware logStdoutDev

  get "/"         $ redirect "/search"
  get "/search"   $ do
    ds <- withIx $ return . Dtt.size . ixDocTable
    html . Tmpl.index $ ds
  get "/add"      $ html Tmpl.addDocs

  -- simple query
  get "/search/:query" $ do
    query    <- param "query"
    res      <- runQuery query
    json res

  -- simple query with paging
  get "/search/:query/:page/:perPage" $ do
    query    <- param "query"
    p        <- param "page"
    pp       <- param "perPage"
    res      <- runQuery query
    case res of
      (JsonSuccess docs) -> json . JsonSuccess $ mkPagedResult docs p pp
      _                  -> json res

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
      let uris = checkApiDocUrisAbsence jss ix :: Either [(URI, DocId)] [URI]
      -- empty intersection of sets -> safe to insert
      return $ either
        (\existingUris ->
            (ix, return . map fst $ existingUris))
        (\_            ->
             ( foldr (\e ix_ -> uncurry (Ix.insert ix_) $ toDocAndWords' e) ix jss
            , Nothing))
        uris
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
      let uris = checkApiDocUrisExistence jss ix :: Either [URI] [(URI, DocId)]
      -- set of new docs minus docTable
      -- difference set is empty -> safe to update
      return $ either
        (\nonexistentUris ->
            (ix, return nonexistentUris))
        (\existentDocs    ->
            ( foldr (\(docId, apiDoc) ix_ -> uncurry (Ix.update ix_ docId) . toDocAndWords' $ apiDoc)
                    ix (flip zip jss . map snd $ existentDocs)
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
            ( foldr (\(docId, apiDoc) ->
                modifyWithDescription (apiDocDescrMap apiDoc) (snd . toDocAndWords' $ apiDoc) docId) ix (flip zip jss . map snd $ existentDocs)
            , Nothing))
        uris

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
