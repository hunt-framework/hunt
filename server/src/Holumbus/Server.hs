{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoMonoLocalBinds  #-}

module Holumbus.Server {-(start)-} where

import           Prelude                                  as P

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class                   (MonadIO, liftIO)

import           Network.Wai.Middleware.RequestLogger

import           Web.Scotty

import qualified Data.Binary                              as Bin
import           Data.Either                              (partitionEithers)
import qualified Data.Map                                 as M
import qualified Data.Set                                 as S
import           Data.Text                                (Text)

import qualified Data.Aeson                               as A
import           Data.Aeson.Encode.Pretty                 (encodePretty)
{--
import           Holumbus.Utility                         ((.::))

import           Holumbus.Index.Common
import qualified Holumbus.Index.Common.DocIdMap           as DM

--import           Holumbus.Index.Common.Document           as Doc
import           Holumbus.Index.Common.CompressedDocument

--import qualified Holumbus.Index.Proxy.CachedIndex         as IxCache
--import qualified Holumbus.Index.Text.Inverted.PrefixMem as Inv
import           Holumbus.Index.TextIndex               (TextIndex)
import qualified Holumbus.Index.TextIndex               as TIx
import           Holumbus.Index.InvertedIndex           as Inv
import qualified Holumbus.DocTable.DocTable             as Dt
import           Holumbus.DocTable.DocTable             hiding (filter, map)
import qualified Holumbus.DocTable.HashedDocuments      as Dt

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Result

import           Holumbus.Indexer.TextIndexer           (Indexer, TextIndexer)
import qualified Holumbus.Indexer.TextIndexer           as Ixx
import           Holumbus.Server.Analyzer
import           Holumbus.Server.Common                   hiding (Query)
import qualified Holumbus.Server.Template                 as Tmpl
--import qualified Holumbus.Server.Interpreter              as Ip
--}
import           Holumbus.Interpreter.Interpreter
-- ----------------------------------------------------------------------------

start :: IO ()
start = main1 $ Search "test"

main1 :: Command -> IO ()
main1 c
    = do env0 <- initEnv emptyIndexer emptyOptions
         let eval = runCmd env0
         eval c >>= print
         return ()


{--
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
{-
newTextIndexer' :: TextIndex i -> DocTable d de -> TextIndexer i d de
newTextIndexer' i = newTextIndexer (IxCache.empty i)
-}

indexer         :: Indexer Inv.InvertedIndex (Dt.Documents Document)
indexer         = Ixx.newInvertedIndexer

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: (Monad m, TextIndex i v, DocTable d, e ~ DValue d) =>
                   i v -> d -> Query -> m (Result e)
runQueryM       = processQueryM queryConfig

-- | Like Web'.Scotty.json', but pretty.
jsonPretty :: (A.ToJSON a) => a -> ActionM ()
jsonPretty v = do
  setHeader "Content-Type" "application/json"
  raw $ encodePretty v


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

-- ----------------------------------------------------------------------------

-- server itself:
--
--  -> should get some kind of state from command line or config file
--     f.e: which index impl to use, which doc store, which persistent backend etc...
start :: IO ()
start = scotty 3000 $ do

  -- interpreter
  -- env <- liftIO $ Ip.initEnv indexer (Ip.CMOptions (toDocAndWords Doc.wrapDoc))
  -- let runCmd = Ip.runCmd env

  -- index
  --let ixM    = Ip.cevIndexer env
  ixM    <- liftIO $ newMVar indexer
  -- {-# LANGUAGE NoMonoLocalBinds #-} needed
  -- -> http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
  let withIx = withIndex' ixM -- :: MonadIO m => (Ixx ... -> IO b)            -> m b
  let modIx_ = modIndex_  ixM -- :: MonadIO m => (Ixx ... -> IO (Ixx ...))    -> m ()
  let modIx  = modIndex   ixM -- :: MonadIO m => (Ixx ... -> IO (Ixx ..., b)) -> m b
  let runQuery queryStr = withIx $ \ix ->
        case parseQuery queryStr of
          (Left err) -> return . JsonFailure . return $ err
          (Right query) ->
            runQueryM (Ixx.ixIndex ix) (Ixx.ixDocTable ix) query
            >>= return . JsonSuccess
                . map (\(_,(DocInfo d _,_)) -> unwrap d)
                . DM.toList . docHits

  -- request / response logging
  middleware logStdoutDev

  get "/"         $ redirect "/search"
  get "/search"   $ do
    ds <- withIx $ return . Dt.size . Ixx.ixDocTable
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
          runQueryM (Ixx.ixIndex ix) (Ixx.ixDocTable ix) query
          >>= return . JsonSuccess
              . map (\ (c, (_, o)) -> (c, M.foldr (\m r -> r + DM.size m) 0 o))
              . M.toList. wordHits
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
             ( foldr (uncurry Ixx.insert . toDocAndWords') ix jss
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


  -- delete a set of documents by URI
  post "/document/delete" $ do
    jss <- jsonData :: ActionM (S.Set URI)
    modIx_ $ \ix -> return $ Ixx.deleteDocsByURI jss ix
    json (JsonSuccess "document(s) deleted" :: JsonResponse Text)


  -- TODO: proper load/save, routes, get/post/.., filenames, exception handling etc.

  let mkIndexerPath = (++ ".ixx")

  -- write the indexer to disk
  get "/binary/save/:filename" $ do
    filename  <- param "filename"
    withIx $ Bin.encodeFile $ mkIndexerPath filename
    json (JsonSuccess "index saved" :: JsonResponse Text)


  -- load indexer from disk
  get "/binary/load/:filename" $ do
    filename  <- param "filename"
    modIx_ $ \_ -> Bin.decodeFile $ mkIndexerPath filename
    json (JsonSuccess "index loaded" :: JsonResponse Text)
{--
  -- interpreter route
  post "/exec" $ do
    -- Raises an exception if parse is unsuccessful
    cmd  <- jsonData :: ActionM Command
    iRes <- liftIO $ runCmd cmd
    either json json iRes
--}
  notFound $ text "page not found"
--}
