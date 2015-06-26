{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hunt.Server.Client
    ( -- * Monad Transformer
      withHuntServer
    , HuntConnectionT (..)
    , ServerAndManager (..)
    , newServerAndManager
    , withServerAndManager
    , HuntClientException

    -- * Conveniece wrapper
    , getAutocomplete
    , postAutocomplete
    , getQuery
    , postQuery
    , postInsert
    , postCommand

    -- * Simple
    , evalOnServer
    , evalOnServer_

    -- * Some Reexports from hunt
    , descriptionToCmd

    -- * misc
    , lowercaseConstructorsOptions
    )
where

import           Control.Applicative          (Applicative, (<$>))
import           Control.Exception            (Exception, throwIO)
import           Control.Monad                (mzero, liftM)
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, ReaderT, ask,
                                               runReaderT)
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Resource (MonadBaseControl, ResourceT,
                                               runResourceT)

import           Data.Aeson                   (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Types             as JSON
import           Data.ByteString.Lazy         (ByteString)
import           Data.Char                    (toLower)
import           Data.Default                 (Default, def)
import           Data.String.Conversions      (cs, (<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable)

import qualified Network.HTTP.Client          as HTTP (defaultManagerSettings)
import qualified Network.HTTP.Conduit         as HTTP
import           Network.HTTP.Types.URI       (urlEncode)

import qualified Hunt.ClientInterface         as H
import qualified Hunt.Common.ApiDocument      as H (ApiDocuments)
-- import qualified Hunt.Interpreter.Command     as H (CmdRes)
-- import qualified Hunt.Common.BasicTypes       as H (RegEx, Score, Weight)
import qualified Hunt.Index.Schema            as H (CNormalizer,
                                                    CTokenizer,
                                                    TokenizerType(..),
                                                    ContextSchema (..),
                                                  ContextType (..))
import qualified Hunt.Interpreter.Command     as H (Command (..))
import           Hunt.Query.Language.Grammar  (Query)


data JsonResponse r
    = JsonSuccess {_jsonValue :: r}
    | JsonFailure Int [Text]
      deriving (Show)

instance (ToJSON r) => ToJSON (JsonResponse r) where
    toJSON (JsonSuccess msg) = JSON.object
        [ "code"  .= (0 :: Int)
        , "msg"   .= msg
        ]

    toJSON (JsonFailure n msg) = JSON.object
        [ "code"  .= n
        , "msg"   .= msg
        ]

instance (FromJSON r) => FromJSON (JsonResponse r) where
    parseJSON (JSON.Object v) = do
        code <- v .: "code"
        case code of
            0 -> JsonSuccess <$> (v .: "msg")
            _ -> JsonFailure code <$> (v .: "msg")
    parseJSON _ = mzero

-- ------------------------------

data ServerAndManager
    = ServerAndManager {
        unServer  :: Text,
        unManager :: HTTP.Manager
      }

newtype HuntConnectionT m a
    = HuntConnectionT { runHuntConnectionT :: ReaderT ServerAndManager (ResourceT m) a }
      deriving (Functor, Applicative, Monad, MonadReader ServerAndManager, MonadIO)

instance MonadTrans HuntConnectionT where
    -- lift :: (Monad m) => m a -> HuntConnectionT m a
    lift = HuntConnectionT . lift . lift

instance (Monad m, MonadIO m) => MonadThrow (HuntConnectionT m) where
    throwM = liftIO . throwIO

data HuntClientException
    = ServerError Int [Text]
    | JSONDecodeError Text
--  | HttpException HTTP.HttpException
      deriving (Show, Typeable)

instance Exception HuntClientException

--instance Failure HuntClientException m => Failure HTTP.HttpException m where
--    failure e = failure (HttpException e)

-- | Creates a new ServerAndManager from a Host
newServerAndManager :: (MonadIO m) => Text -> m ServerAndManager
newServerAndManager s = do
    m <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    return $ ServerAndManager (checkServerUrl s) m

-- | runs a HuntConnectionT with a ServerAndManager
withServerAndManager :: MonadBaseControl IO m => ServerAndManager -> HuntConnectionT m a -> m a
withServerAndManager sm x
    = runResourceT $ (runReaderConnectionT sm x)

-- | runs a HuntConnectionT in a monad
withHuntServer :: (MonadIO m, MonadBaseControl IO m) => Text -> HuntConnectionT m a -> m a
withHuntServer s x
    = HTTP.withManager $ \man -> (runReaderConnectionT  (ServerAndManager (checkServerUrl s) man)) x

runReaderConnectionT :: ServerAndManager -> HuntConnectionT m a -> ResourceT m a
runReaderConnectionT sm x
    = (runReaderT . runHuntConnectionT) x sm

-- ---------------------------

checkServerUrl :: Text -> Text
checkServerUrl s
    | T.null s
        = "http://localhost:3000/"
    | '/' == T.last s
        = s
    | otherwise
        = s <> "/"

encodeRequest :: Text -> Text
encodeRequest = cs . urlEncode False . cs
--encodeRequest = under utf8 $ urlEncode False

makeRequest :: (MonadIO m) => Text -> HuntConnectionT m HTTP.Request
makeRequest path = do
    sm <- ask
    HTTP.parseUrl $ cs $ T.concat [ unServer sm, path]

httpLbs :: (FromJSON a, MonadIO m, MonadThrow m) => HTTP.Request -> HuntConnectionT m a
httpLbs request = do
    sm <- ask
    response <- HTTP.httpLbs request (unManager sm)
    handleJsonResponse (HTTP.responseBody response)

handleAutoCompeteResponse :: (Monad m) => [(Text, H.Score)] -> m [Text]
handleAutoCompeteResponse res = return $ liftM fst $ res

getAutocomplete :: (MonadIO m, MonadThrow m) => Text -> HuntConnectionT m [Text]
getAutocomplete q = do
    request <- makeRequest
                    $ T.concat [ "completion/", encodeRequest q, "/20"]
    result <- (httpLbs request)
    handleAutoCompeteResponse result

getQuery :: (MonadIO m, MonadThrow m, FromJSON r) => Text -> Int -> Int -> HuntConnectionT m  (H.LimitedResult r)
getQuery query' maxResults offset = do
    request <- makeRequest
                    $ T.concat [ "search/", encodeRequest query', "/", cs $ show offset, "/", cs $ show maxResults]
    httpLbs request

postInsert :: (MonadIO m, MonadThrow m) => H.ApiDocuments -> HuntConnectionT m Text
postInsert docs
    = postCommand $ H.cmdSequence $ H.cmdInsertDoc <$> docs

postAutocomplete :: (MonadIO m, MonadThrow m) => Query -> HuntConnectionT m [Text]
postAutocomplete qq  = do
    result <- postCommand $ H.setMaxResults 20 $ H.cmdCompletion qq
    handleAutoCompeteResponse result

postQuery :: (MonadIO m, MonadThrow m, FromJSON r) => Query -> Int -> HuntConnectionT m (H.LimitedResult r)
postQuery q offset = do
    postCommand $ H.setResultOffset offset $ H.setMaxResults 20 $ H.cmdSearch q

postCommand :: (FromJSON a, MonadIO m, MonadThrow m) => H.Command -> HuntConnectionT m a
postCommand cmd = do
    request' <- makeRequest "eval"
    let body    = JSON.encode cmd
        request = request'
                  { HTTP.method = "POST"
                  , HTTP.requestBody = HTTP.RequestBodyLBS body
                  }
    httpLbs request

postCommand_ :: (MonadIO m, MonadThrow m) => H.Command -> HuntConnectionT m JSON.Value
postCommand_ = postCommand

handleJsonResponse :: (FromJSON a, Monad m, MonadThrow m) => ByteString -> m a
handleJsonResponse r
    = safeDecodeJson r >>= unJsonResponse
    where
      unJsonResponse (JsonSuccess r')    = return r'
      unJsonResponse (JsonFailure c err) = throwM $ ServerError c err

      safeDecodeJson bs
          = case JSON.eitherDecode bs of
              (Right  a) -> return a
              (Left err) -> throwM $ JSONDecodeError $ cs $ ("Json decode error: " <> err)

-- | send a command to a hunt server
evalOnServer :: (MonadIO m, MonadBaseControl IO m, FromJSON a, Monad m, MonadThrow m) => Text -> H.Command -> m a
evalOnServer server cmd = withHuntServer server (postCommand cmd)


evalOnServer_ :: (MonadIO m, MonadBaseControl IO m, Monad m, MonadThrow m) => Text -> H.Command -> m JSON.Value
evalOnServer_ = evalOnServer


data ContextType = TextContext | DateContext | PositionContext | IntContext
    deriving(Show, Enum, Eq)

-- | The type can be any of the supported basic index types.
--   The regexp validates and splits the text into words:
--     []  -> invalid
--     xs  -> words/tokens
--   Every type can have a type-specific regexp.
--     => This means is has to match both the type-specific and the context-specific regexp.
--        Example: A CDate text has to match the type-specific regexp (XMLSchema-Date)
--                 (requirement for the corresponding Date-Parser which is used to normalize)
--   Every type can have a type-specific normalizer.
--     => This means it is first transformed by the type-specific normalizer and then by the
--        context-specific normalizers
--
--   /TL;DR/
--   Every input for both search and insert has
--     - two regexps    for validation and tokenization
--     - two normalizer for transformation
--   The first  regexp/normalizer is type-specific and is applied first (forced)
--   The second regexp/normalizer is context-specific (defined/chosen by user)

data ContextDescription
    = ContextDescription
    { -- optional regex to overwrite default given by context type
      cxTokenizer  :: Maybe H.CTokenizer
      -- normalizers to apply
    , cxNormalizer :: [H.CNormalizer]
      -- context weight
    , cxWeight     :: H.Weight
      -- should this context used in non-context queries?
    , cxDefault    :: Bool
      -- contexttype
    , cxType       :: ContextType
      -- name
    , cxName       :: Text
    }
    deriving Show

instance Default ContextDescription where
    def = ContextDescription Nothing [] 1.0 True TextContext ""

descriptionToCmd :: ContextDescription -> H.Command
descriptionToCmd d
    = H.InsertContext {H.icIContext = cxName d, H.icSchema = schema}
    where
      schema
          = H.ContextSchema
            { H.cxTokenizer  = cxTokenizer  d
            , H.cxNormalizer = cxNormalizer d
            , H.cxWeight     = cxWeight     d
            , H.cxDefault    = cxDefault    d
            , H.cxType       = contextType
            }
      contextType
          = def { H.ctName = name $ cxType d }

      name TextContext     = "text"
      name DateContext     = "date"
      name PositionContext = "position"
      name IntContext      = "int"

{-# DEPRECATED descriptionToCmd "Don't use these" #-}

--capitalize :: String -> String
--capitalize = over _head toUpper . over (_tail.each) toLower

lowercase :: String -> String
lowercase = map toLower

lowercaseConstructorsOptions :: JSON.Options
lowercaseConstructorsOptions
    = JSON.Options
      { JSON.fieldLabelModifier      = id
      , JSON.constructorTagModifier  = lowercase
      , JSON.allNullaryToStringTag   = True
      , JSON.omitNothingFields       = False
      , JSON.sumEncoding             = JSON.defaultTaggedObject
      }

-- ---------------------------
