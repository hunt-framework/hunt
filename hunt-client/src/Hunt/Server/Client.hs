{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving, PackageImports #-}

module Hunt.Server.Client (
    -- * Monad Transformer
      withHuntServer
    , HuntConnectionT (..)
    , ServerAndManager (..)
    , newServerAndManager
    , withServerAndManager

   -- * Conveniece wrapper
    , autocomplete
    , query
    , insert
    , eval

    -- * Some Reexports from hunt
    , H.LimitedResult (..)
    , H.ApiDocument (..)
    , H.position
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

import           Data.Either ()
import           Data.Char (isSpace, {- toUpper, -}toLower)
import           Data.String ()

import           Data.Default (Default, def)

-- import Control.Arrow (first)
import           Control.Monad (mzero, (>=>))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)

import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE

import           Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as BL

import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import           Control.Lens (over, both, _Left, {-_head, _tail, -}each, Mutator, Each, under)
-- import           System.IO (utf8)

import           Data.Conduit (runResourceT, ResourceT, MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTP (defaultManagerSettings)

import           Network.HTTP.Types.URI (urlEncode)

import           Control.Failure (Failure)

import qualified Hunt.Common.ApiDocument as H
import qualified Hunt.Index.Schema.Normalize.Position as H
import qualified Hunt.Interpreter.Command as H (Command(..), CmdResult (..))
import qualified Hunt.Index.Schema as H (CRegex, CNormalizer, CWeight, ContextSchema (..), ContextType (..))
import           Hunt.Common.BasicTypes


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
    deriving (Monad, MonadReader ServerAndManager, MonadIO)

instance MonadTrans HuntConnectionT where
    -- lift :: (Monad m) => m a -> HuntConnectionT m a
    lift = HuntConnectionT . lift . lift

-- | Creates a new ServerAndManager from a Host
newServerAndManager :: Text -> IO ServerAndManager
newServerAndManager s = do
    m <- HTTP.newManager HTTP.defaultManagerSettings
    return $ ServerAndManager (checkServerUrl s) m

-- | runs a HuntConnectionT with a ServerAndManager
withServerAndManager :: MonadBaseControl IO m =>  HuntConnectionT m a -> ServerAndManager -> m a
withServerAndManager x = runResourceT . runReaderConnectionT x

-- | runs a HuntConnectionT in a monad
withHuntServer :: (MonadIO m, MonadBaseControl IO m) => HuntConnectionT m a -> Text -> m a
withHuntServer x s = HTTP.withManager (runReaderConnectionT x . ServerAndManager (checkServerUrl s))


runReaderConnectionT :: HuntConnectionT m a -> ServerAndManager -> ResourceT m a
runReaderConnectionT x sm = (runReaderT . runHuntConnectionT) x sm

checkServerUrl :: Text -> Text
checkServerUrl s
    | T.null s = "http://localhost:3000/"
    | '/' == T.last s = s
    | otherwise = s <> "/"

encodeRequest :: Text -> Text
encodeRequest = cs . urlEncode False . cs
--encodeRequest = under utf8 $ urlEncode False

makeRequest :: (MonadIO m, Failure HTTP.HttpException m) => Text -> HuntConnectionT m HTTP.Request
makeRequest path = do
    sm <- ask
    HTTP.parseUrl $ cs $ T.concat [ unServer sm, path]

httpLbs :: (MonadIO m) => HTTP.Request -> HuntConnectionT m ByteString
httpLbs request = do
    sm <- ask
    response <- HTTP.httpLbs request (unManager sm)
    return $ HTTP.responseBody response



autocomplete :: (MonadIO m, Failure HTTP.HttpException m) => Text  -> HuntConnectionT m (Either Text [Text])
autocomplete q = do
    request <- makeRequest $ T.concat [ "completion/", encodeRequest q, "/20"]
    d <- httpLbs request
    return $ (eitherDecodeT >=> handleJsonResponse >=> filterByRest >=> prefixWith) d
    where
        (rest, prefix) = over both T.reverse $ T.span (\ c -> (not $ isSpace c) && (c /= ':') && (c /= '!') && (c /= '~')) $ T.reverse q

        filterByRest :: [(Text, [Text])] -> Either Text [Text]
        filterByRest = return . map fst . filter (\(_, rs) -> rest `elem` rs)

        prefixWith :: [Text] -> Either Text [Text]
        prefixWith = ((return .) . fmap) $ (prefix <>)


query :: (MonadIO m, FromJSON r, Failure HTTP.HttpException m) => Text -> HuntConnectionT m  (Either Text (H.LimitedResult r))
query q = do
    request <- makeRequest $ T.concat [ "search/", encodeRequest q, "/0/20"]
    d <- httpLbs request
    return $ (eitherDecodeT >=> handleJsonResponse) d

insert :: (MonadIO m, Failure HTTP.HttpException m) => H.ApiDocuments -> HuntConnectionT m Text
insert docs = eval $ map H.Insert docs

eval :: (MonadIO m, Failure HTTP.HttpException m) => [H.Command] -> HuntConnectionT m Text
eval cmds = do
    request' <- makeRequest "eval"
    let body = JSON.encode cmds
        request = request' { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS body}
    httpLbs request >>= return . cs

handleJsonResponse :: (FromJSON b) => JsonResponse b -> Either Text b
handleJsonResponse (JsonSuccess r) = Right r
handleJsonResponse (JsonFailure c err) = Left $ T.concat $ ["Code: ", cs $ show c, " "] ++ err

eitherDecodeT :: FromJSON a => ByteString -> Either Text a
eitherDecodeT =   over _Left (("Json decode error: " <>) . cs) . JSON.eitherDecode


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
    cxRegEx      :: Maybe H.CRegex
    -- normalizers to apply
    , cxNormalizer :: [H.CNormalizer]
    -- context weight
    , cxWeight     :: H.CWeight
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

lowercase :: Each Mutator s t Char Char => s -> t
lowercase = over (each) toLower

lowercaseConstructorsOptions :: JSON.Options
lowercaseConstructorsOptions = JSON.Options {
       JSON.fieldLabelModifier      = id
     , JSON.constructorTagModifier  = lowercase
     , JSON.allNullaryToStringTag   = True
     , JSON.omitNothingFields       = False
     , JSON.sumEncoding             = JSON.defaultTaggedObject
}
