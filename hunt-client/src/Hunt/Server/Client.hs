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
    , autocomplete
    , query
    , insert
    , eval
    , evalAutocomplete
    , evalQuery

    -- * Simple
    , evalOnServer

    -- * Some Reexports from hunt
    , descriptionToCmd

    -- * misc
    , lowercaseConstructorsOptions
    )
where

import           Control.Applicative          (Applicative, (<$>))
import           Control.Exception            (Exception, throwIO)
import           Control.Monad                (mzero)
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
-- import qualified Hunt.Common.BasicTypes       as H (RegEx, Score, Weight)
import qualified Hunt.Index.Schema            as H (CNormalizer,
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
            0 -> do
                msg <- v .: "msg"
                return $ JsonSuccess msg
            _ -> do
                msg <- v .: "msg"
                return $ JsonFailure code msg
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
withServerAndManager :: MonadBaseControl IO m =>  HuntConnectionT m a -> ServerAndManager -> m a
withServerAndManager x
    = runResourceT . runReaderConnectionT x

-- | runs a HuntConnectionT in a monad
withHuntServer :: (MonadIO m, MonadBaseControl IO m) => HuntConnectionT m a -> Text -> m a
withHuntServer x s
    = HTTP.withManager (runReaderConnectionT x . ServerAndManager (checkServerUrl s))

runReaderConnectionT :: HuntConnectionT m a -> ServerAndManager -> ResourceT m a
runReaderConnectionT x sm
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

httpLbs :: (MonadIO m) => HTTP.Request -> HuntConnectionT m ByteString
httpLbs request = do
    sm <- ask
    response <- HTTP.httpLbs request (unManager sm)
    return $ HTTP.responseBody response

handleAutoCompeteResponse :: (Monad m, MonadThrow m) => ByteString -> m [Text]
handleAutoCompeteResponse result
    = handleJsonResponse result >>= return . toWordList
    where
      toWordList :: [(Text, H.Score)] -> [Text]
      toWordList = map fst

autocomplete :: (MonadIO m) => Text -> HuntConnectionT m [Text]
autocomplete q
    = do request <- makeRequest
                    $ T.concat [ "completion/", encodeRequest q, "/20"]
         d <- httpLbs request
         handleAutoCompeteResponse d

query :: (MonadIO m, FromJSON r) => Text -> Int -> Int -> HuntConnectionT m  (H.LimitedResult r)
query query' maxResults offset
    = do request <- makeRequest
                    $ T.concat [ "search/", encodeRequest query', "/", cs $ show offset, "/", cs $ show maxResults]
         httpLbs request >>= handleJsonResponse

insert :: (MonadIO m) => H.ApiDocuments -> HuntConnectionT m ByteString
insert docs
    = eval $ H.cmdSequence $ H.cmdInsertDoc <$> docs

evalAutocomplete :: (MonadIO m) => Query -> HuntConnectionT m [Text]
evalAutocomplete qq
    = do result <- eval $ H.setMaxResults 20 $ H.cmdCompletion qq
         handleAutoCompeteResponse result

evalQuery :: (MonadIO m, FromJSON r) => Query -> Int -> HuntConnectionT m (H.LimitedResult r)
evalQuery q offset = do
    result <- eval $ H.setResultOffset offset $ H.setMaxResults 20 $ H.cmdSearch q
    handleJsonResponse result

eval :: (MonadIO m) => H.Command -> HuntConnectionT m ByteString
eval cmd = do
    request' <- makeRequest "eval"
    let body    = JSON.encode cmd
        request = request'
                  { HTTP.method = "POST"
                  , HTTP.requestBody = HTTP.RequestBodyLBS body
                  }
    httpLbs request


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
evalOnServer :: (MonadIO m, MonadBaseControl IO m) => Text -> H.Command -> m ByteString
evalOnServer server cmd = withHuntServer (eval cmd) server        


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
      cxRegEx      :: Maybe H.RegEx
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
            { H.cxRegEx      = cxRegEx      d
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
