{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module Hayoo.HolumbusClient where

import GHC.Generics (Generic)

--import Data.Char
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as T

--import Control.Lens

import Data.Aeson
--import Data.Aeson.Types

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

autocomplete :: (MonadIO m) => Text -> Text  -> m (Either Text [Text])
autocomplete = H.autocomplete

query :: (MonadIO m) => Text -> Text -> m (Either Text (H.LimitedResult SearchResult))
query = H.query