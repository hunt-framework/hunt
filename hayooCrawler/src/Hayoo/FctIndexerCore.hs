{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------

module Hayoo.FctIndexerCore
where

import           Control.DeepSeq

-- import           Data.Aeson
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as B
import qualified Data.ByteString.Lazy         as LB
-- import           Data.Maybe

import           Hayoo.FunctionInfo
import           Hayoo.IndexTypes

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
-- import           Holumbus.Index.Common        hiding (URI)

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type FctCrawlerConfig = IndexCrawlerConfig () DummyDocs FunctionInfo
type FctCrawlerState  = IndexCrawlerState  () DummyDocs FunctionInfo

type FctIndexerState  = IndexerState       () DummyDocs FunctionInfo

newtype DummyDocs a = DummyDocs ()

instance NFData (DummyDocs a)

instance Binary (DummyDocs a) where
    put _ = return ()
    get   = return emptyDummyDocs

emptyDummyDocs :: DummyDocs a
emptyDummyDocs = DummyDocs ()

emptyFctState :: FctIndexerState
emptyFctState = emptyIndexerState () emptyDummyDocs

-- ------------------------------------------------------------

unionHayooFctStatesM        :: FctIndexerState -> FctIndexerState -> IO FctIndexerState
unionHayooFctStatesM _ixs1 _ixs2
    = return emptyFctState

insertHayooFctM :: (URI, RawDoc FunctionInfo) -> FctIndexerState -> IO FctIndexerState
insertHayooFctM rd@(_rawUri, (rawContexts, rawTitle, _rawCustom)) ixs
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise                 = do flushFctDoc
                                     return ixs
    where
    nullContexts                = and . map (null . snd) $ rawContexts
    path                        = "functions/" ++ rawTitle ++ ".json"
    flushFctDoc                 = flushRawCrawlerDoc (LB.writeFile path) (RCD rd)

-- ------------------------------------------------------------

-- the pkgIndex crawler configuration

indexCrawlerConfig           :: SysConfig                                    -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the filter for computing the document title, default is empty string
                                -> Maybe (IOSArrow XmlTree FunctionInfo)         -- ^ the filter for the cutomized doc info, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> FctCrawlerConfig                             -- ^ result is a crawler config

indexCrawlerConfig
    = indexCrawlerConfig' insertHayooFctM unionHayooFctStatesM

-- ------------------------------------------------------------
