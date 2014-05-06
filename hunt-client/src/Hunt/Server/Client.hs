{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}

module Hunt.Server.Client (
    -- * Monad Transformer
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

    -- * Some Reexports from hunt
    , H.LimitedResult (..)
    , H.ApiDocument (..)
    , ContextDescription (..)
    , ContextType (..)
    , def
    , H.Command (..)
    , H.CmdResult (..)
    , descriptionToCmd

    -- * misc
    , lowercaseConstructorsOptions
)
where

import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import           Data.ByteString.Lazy (ByteString)
import           Data.Char (isSpace, {- toUpper, -}toLower)
import           Data.Default (Default, def)
import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           Control.Applicative (Applicative)
import           Control.Exception (Exception, throwIO)
import           Control.Monad (mzero)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadBaseControl)
import           Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)


import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTP (defaultManagerSettings)
import           Network.HTTP.Types.URI (urlEncode)

import qualified Hunt.Common.ApiDocument as H
import qualified Hunt.Common.BasicTypes as H (RegEx, Weight)
import qualified Hunt.Interpreter.Command as H (Command(..), CmdResult (..))
import qualified Hunt.Index.Schema as H (CNormalizer, ContextSchema (..), ContextType (..))
import           Hunt.Query.Language.Grammar (Query) -- (..), BinOp (..), TextSearchType (..))


data JsonResponse r =
      JsonSuccess {_jsonValue :: r}
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
data ServerAndManager = ServerAndManager {
    unServer :: Text,
    unManager :: HTTP.Manager
}

newtype HuntConnectionT m a = HuntConnectionT { runHuntConnectionT :: ReaderT ServerAndManager (ResourceT m) a }
    deriving (Functor, Applicative, Monad, MonadReader ServerAndManager, MonadIO)

instance MonadTrans HuntConnectionT where
    -- lift :: (Monad m) => m a -> HuntConnectionT m a
    lift = HuntConnectionT . lift . lift

instance (Monad m, MonadIO m) => MonadThrow (HuntConnectionT m) where
    throwM = liftIO . throwIO

data HuntClientException =
      ServerError Int [Text]
--    | HttpException HTTP.HttpException
    | JSONDecodeError Text
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
withServerAndManager x = runResourceT . runReaderConnectionT x

-- | runs a HuntConnectionT in a monad
withHuntServer :: (MonadIO m, MonadBaseControl IO m) => HuntConnectionT m a -> Text -> m a
withHuntServer x s = HTTP.withManager (runReaderConnectionT x . ServerAndManager (checkServerUrl s))


runReaderConnectionT :: HuntConnectionT m a -> ServerAndManager -> ResourceT m a
runReaderConnectionT x sm = (runReaderT . runHuntConnectionT) x sm

 -- ---------------------------

checkServerUrl :: Text -> Text
checkServerUrl s
    | T.null s = "http://localhost:3000/"
    | '/' == T.last s = s
    | otherwise = s <> "/"

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

handleAutoCompeteResponse :: (Monad m, MonadThrow m) => Text -> ByteString -> m [Text]
handleAutoCompeteResponse q result = do
    response <- handleJsonResponse result
    return $ prefixWith $ filterByRest response
    where
        (rest, prefix) = mapTuple T.reverse $ T.span (\ c -> (not $ isSpace c) && (c /= ':') && (c /= '!') && (c /= '~')) $ T.reverse q

        filterByRest :: [(Text, [Text])] -> [Text]
        filterByRest = map fst . filter (\(_, rs) -> rest `elem` rs)

        prefixWith :: [Text] -> [Text]
        prefixWith x = map (prefix <> ) x

autocomplete :: (MonadIO m) => Text  -> HuntConnectionT m [Text]
autocomplete q = do
    request <- makeRequest $ T.concat [ "completion/", encodeRequest q, "/20"]
    d <- httpLbs request
    handleAutoCompeteResponse q d


query :: (MonadIO m, FromJSON r) => Text -> Int -> HuntConnectionT m  (H.LimitedResult r)
query query' offset = do
    request <- makeRequest $ T.concat [ "search/", encodeRequest query', "/", cs $ show offset, "/20"]
    httpLbs request >>= handleJsonResponse

insert :: (MonadIO m) => H.ApiDocuments -> HuntConnectionT m ByteString
insert docs = eval $ map H.Insert docs

evalAutocomplete :: (MonadIO m) => Text -> Query -> HuntConnectionT m [Text]
evalAutocomplete qt qq = do
    result <- eval $ [H.Completion qq 20]
    handleAutoCompeteResponse qt result

evalQuery :: (MonadIO m, FromJSON r) => Query -> Int -> HuntConnectionT m (H.LimitedResult r)
evalQuery query' offset = do
    result <- eval $ [H.Search query' offset 20 False Nothing]
    handleJsonResponse result

eval :: (MonadIO m) => [H.Command] -> HuntConnectionT m ByteString
eval cmds = do
    request' <- makeRequest "eval"
    let body = JSON.encode cmds
        request = request' { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS body}
    httpLbs request

handleJsonResponse :: (FromJSON a, Monad m, MonadThrow m) => ByteString -> m a
handleJsonResponse r = safeDecodeJson r >>= unJsonResponse
    where
    unJsonResponse (JsonSuccess r') = return r'
    unJsonResponse (JsonFailure c err) = throwM $ ServerError c err

    safeDecodeJson bs = case JSON.eitherDecode bs of
        (Right a) -> return a
        (Left err) -> throwM $ JSONDecodeError $ cs $ ("Json decode error: " <> err)


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
data ContextDescription = ContextDescription
    {
    -- optional regex to overwrite default given by context type
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
descriptionToCmd d = H.InsertContext {H.icIContext = cxName d, H.icSchema = schema}
    where
        schema = H.ContextSchema {
            H.cxRegEx = cxRegEx d,
            H.cxNormalizer = cxNormalizer d,
            H.cxWeight = cxWeight d,
            H.cxDefault = cxDefault d,
            H.cxType = contextType
        }
        contextType = def { H.ctName = name $ cxType d }
        name TextContext = "text"
        name DateContext = "date"
        name PositionContext = "position"
        name IntContext = "int"


--capitalize :: String -> String
--capitalize = over _head toUpper . over (_tail.each) toLower

lowercase :: String -> String
lowercase = map toLower

lowercaseConstructorsOptions :: JSON.Options
lowercaseConstructorsOptions = JSON.Options {
       JSON.fieldLabelModifier      = id
     , JSON.constructorTagModifier  = lowercase
     , JSON.allNullaryToStringTag   = True
     , JSON.omitNothingFields       = False
     , JSON.sumEncoding             = JSON.defaultTaggedObject
}

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)