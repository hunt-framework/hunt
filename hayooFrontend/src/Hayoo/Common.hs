{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hayoo.Common where

import GHC.Generics (Generic)

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)

import Data.Text.Lazy (Text)
import Data.Aeson

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import "mtl" Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)

import qualified Holumbus.Server.Client as H

data ResultType = Function | Class | Data | Module | Package | Newtype | Type | Method
    deriving (Eq, Show, Generic)


instance FromJSON ResultType where
     parseJSON = genericParseJSON H.lowercaseConstructorsOptions


data SearchResult = SearchResult {
    uri :: Text, 
    resultPackage :: Text,
    resultModule :: Text,
    resultName :: Text,
    resultSignature :: Text,
    resultDescription :: Text,
    resultSource :: Text,
    resultType :: ResultType
} deriving (Show, Eq)

instance FromJSON SearchResult where
    parseJSON (Object v) = do
        u <- v .: "uri" 
        (Object descr) <- v .: "desc"
        p  <- descr .: "package"
        m  <- descr .: "module"
        n  <- descr .: "name"
        s  <- descr .: "signature"
        d  <- descr .:? "description" .!= ""
        c  <- descr .:? "source" .!= ""
        t  <- descr .:? "type" .!= Function
        return $ SearchResult u p m n s d c t
    parseJSON _ = mzero

newtype HayooServer a = HayooServer { runHayooServer :: ReaderT H.ServerAndManager IO a }
    deriving (Monad, MonadIO, MonadReader (H.ServerAndManager))

hayooServer :: MonadTrans t => HayooServer a -> t HayooServer a
hayooServer = lift 

runHayooReader :: HayooServer a -> H.ServerAndManager -> IO a
runHayooReader = runReaderT . runHayooServer

withServerAndManager' :: H.HolumbusConnectionT IO b -> HayooServer b
withServerAndManager' x = do
    sm <- ask
    --sm <- liftIO $ STM.readTVarIO var
    liftIO $ H.withServerAndManager x sm

autocomplete :: Text -> HayooServer (Either Text [Text])
autocomplete q = withServerAndManager' $ H.autocomplete q

query :: Text -> HayooServer (Either Text (H.LimitedResult SearchResult))
query q = withServerAndManager' $ H.query q