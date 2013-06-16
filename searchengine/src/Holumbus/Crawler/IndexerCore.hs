{-# OPTIONS -XFlexibleContexts -XBangPatterns #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.IndexerCore
    ( RawDoc
    , RawContexts
    , RawContext
    , RawWords
    , RawWord
    , IndexCrawlerConfig
    , IndexContextConfig(..)
    , IndexerState(..)
    , emptyIndexerState
    , indexCrawlerConfig
    , stdIndexer
    , unionIndexerStatesM
    , insertRawDocM
    )
where

-- ------------------------------------------------------------

import           Control.DeepSeq
import           Control.Monad                  ( foldM )
import           Control.Monad.Trans            -- ( MonadIO )

import           Data.Binary                    ( Binary )
import qualified Data.Binary                    as B
import           Data.Function.Selector
import           Data.Maybe
import           Data.Map                       (Map)
--import qualified Data.Map                       as M
import           Data.Text                      ( Text )
import qualified Data.Text                      as T

import           Holumbus.Crawler

import           Holumbus.Index.Common          hiding ( URI )

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

type RawDoc                     = (RawContexts, RawDescription)      -- c is the user defined custom info
type RawContexts                = [RawContext]
type RawContext                 = (Context, RawWords)
type RawWords                   = [RawWord]
type RawWord                    = (Word, Position)
type RawDescription             = Map Text Text -- XXX: split up?

type IndexCrawlerConfig i d     = CrawlerConfig RawDoc (IndexerState i d)
type IndexCrawlerState  i d     = CrawlerState         (IndexerState i d)

data IndexContextConfig         = IndexContextConfig
                                  { ixc_name           :: String
                                  , ixc_collectText    :: IOSArrow XmlTree String
                                  , ixc_textToWords    :: String -> [String]
                                  , ixc_boringWord     :: String -> Bool
                                  }

data IndexerState i d           = IndexerState
                                  { ixs_index           :: ! i          -- the index type
                                  , ixs_documents       :: ! d          -- the type for document descriptions
                                  } deriving (Show)

-- ------------------------------------------------------------

instance (NFData i, NFData d) => NFData (IndexerState i d)
    where
    rnf IndexerState { ixs_index     = i
                     , ixs_documents = d
                     }          = rnf i `seq` rnf d

-- ------------------------------------------------------------

instance (Binary i, Binary d) => Binary (IndexerState i d)
    where
    put s                       = B.put (ixs_index s)
                                  >>
                                  B.put (ixs_documents s)
    get                         = do
                                  ix <- B.get
                                  dm <- B.get
                                  return $ IndexerState
                                             { ixs_index          = ix
                                             , ixs_documents      = dm
                                             }

-- ------------------------------------------------------------

instance (XmlPickler i, XmlPickler d) => XmlPickler (IndexerState i d)
    where
    xpickle                     = xpElem "index-state" $
                                  xpWrap ( uncurry IndexerState
                                         , \ ix -> (ixs_index ix, ixs_documents ix)
                                         ) $
                                  xpPair xpickle xpickle

-- ------------------------------------------------------------

emptyIndexerState               :: i -> d -> IndexerState i d
emptyIndexerState eix edm       = IndexerState
                                  { ixs_index           = eix
                                  , ixs_documents       = edm
                                  }

-- ------------------------------------------------------------

stdIndexer                      :: ( Binary i
                                   , Binary d
                                   , HolIndexM IO i
                                   , HolDocuments d
                                   , NFData i
                                   , NFData d) =>
                                   IndexCrawlerConfig i d       -- ^ adapt configuration to special needs,
                                                                --   use id if default is ok
                                -> Maybe String                 -- ^ resume from interrupted index run with state
                                                                --   stored in file
                                -> [URI]                        -- ^ start indexing with this set of uris
                                -> IndexerState i d             -- ^ the initial empty indexer state
                                -> IO (IndexCrawlerState i d)   -- ^ result is a state consisting of the index and the map of indexed documents

stdIndexer config resumeLoc startUris eis
                                = execCrawler action config (initCrawlerState eis)
    where
    action                      = do
                                  noticeC "indexerCore" ["indexer started"]
                                  res <- maybe (crawlDocs startUris) crawlerResume $ resumeLoc
                                  noticeC "indexerCore" ["indexer finished"]
                                  return res

-- ------------------------------------------------------------

-- general HolIndexM IO i version, for old specialized version see code at end of this file

indexCrawlerConfig              :: ( HolIndexM IO i
                                   , HolDocuments d
                                   , HolDocIndex  d i
                                   , NFData i
                                   , NFData d
                                   ) =>
                                   SysConfig                                    -- ^ document read options
                                -> (URI -> Bool)                                -- ^ the filter for deciding, whether the URI shall be processed
                                -> Maybe (IOSArrow XmlTree String)              -- ^ the document href collection filter, default is 'Holumbus.Crawler.Html.getHtmlReferences'
                                -> Maybe (IOSArrow XmlTree XmlTree)             -- ^ the pre document filter, default is the this arrow
                                -> Maybe (IOSArrow XmlTree Description)         -- ^ the filter for the doc description, default Nothing
                                -> [IndexContextConfig]                         -- ^ the configuration of the various index parts
                                -> IndexCrawlerConfig i d                       -- ^ result is a crawler config

indexCrawlerConfig opts followRef getHrefF preDocF descF0 contextCs
                                = addSysConfig (defaultOpts >>> opts)           -- install the default read options
                                  >>>
                                  ( setS theFollowRef followRef )
                                  >>>
                                  ( setS theProcessRefs   $ fromMaybe getHtmlReferences getHrefF )
                                  >>>
                                  ( setS thePreDocFilter  $ fromMaybe checkDocumentStatus preDocF )     -- in case of errors throw away any contents
                                  >>>
                                  ( setS theProcessDoc rawDocF )                -- rawDocF is build up by the context config, text, title and custom
                                  >>>
                                  enableRobotsTxt                               -- add the robots stuff at the end
                                  >>>                                           -- the filter wrap the other filters
                                  addRobotsNoFollow
                                  >>>
                                  addRobotsNoIndex
                                  $
                                  defaultCrawlerConfig insertRawDocM unionIndexerStatesM
                                                                                -- take the default crawler config
                                                                                -- and set the result combining functions
    where
    rawDocF                     = ( listA contextFs
                                    &&&
                                    descF
                                  ) >>^ (\ (x2, x1) -> (x2, x1))

    -- FIXME: add description filter
    --customF                     = ( fromMaybe none customF0 ) >. listToMaybe
    descF                       = none

    contextFs                   :: IOSArrow XmlTree RawContext
    contextFs                   = catA . map contextF $ contextCs               -- collect all contexts

    contextF                    :: IndexContextConfig -> IOSArrow XmlTree RawContext
    contextF ixc                = constA (T.pack $ ixc_name ixc)                         -- the name of the raw context
                                  &&&
                                  ( ixc_collectText ixc >. processText )        -- the list of words and positions of the collected text
        where                                                                   -- this arrow is deterministic, it always delivers a single pair
        processText             :: [String] -> RawWords
        processText             = concat
                                  >>>
                                  ixc_textToWords ixc
                                  >>>
                                  flip zip [1..]
                                  >>>
                                  filter (fst >>> ixc_boringWord ixc >>> not)
			          >>>
                                  map (first T.pack)

    defaultOpts                 = withRedirect yes
                                  >>>
                                  withAcceptedMimeTypes ["text/html", "text/xhtml"]
                                  >>>
                                  withInputEncoding isoLatin1
                                  >>>
                                  withEncodingErrors no                                  -- encoding errors and parser warnings are boring
                                  >>>
                                  withValidate no
                                  >>>
                                  withParseHTML yes
                                  >>>
                                  withWarnings no

-- ------------------------------------------------------------

unionIndexerStatesM             :: ( MonadIO m
                                   , HolIndexM m i
                                   , HolDocuments d
                                   , HolDocIndex d i
                                   ) =>
                                   IndexerState i d
                                -> IndexerState i d
                                -> m (IndexerState i d)
unionIndexerStatesM ixs1 ixs2
    = return
      $! IndexerState { ixs_index        = ix
                      , ixs_documents    = dt
                      }
    where
      (! dt, ! ix)              = unionDocIndex dt1 ix1 dt2 ix2
      ix1                       = ixs_index     ixs1
      ix2                       = ixs_index     ixs2
      dt1                       = ixs_documents ixs1
      dt2                       = ixs_documents ixs2

-- ------------------------------------------------------------

insertRawDocM                   :: ( MonadIO m
                                   , HolIndexM m i
                                   , HolDocuments d
                                   , NFData i
                                   , NFData d
                                   ) =>
                                   (URI, RawDoc)                                -- ^ extracted URI and doc info
                                -> IndexerState i d                             -- ^ old indexer state
                                -> m (IndexerState i d)                         -- ^ new indexer state

insertRawDocM (rawUri, (rawContexts, rawDescription)) ixs
    | nullContexts              = return ixs    -- no words found in document,
                                                -- so there are no refs in index
                                                -- and document is thrown away
    | otherwise                 = do
                                  newIx  <- foldM (insertRawContextM did) (ixs_index ixs) $ rawContexts
                                  newIxs <- return $
                                            IndexerState { ixs_index        = newIx
                                                         , ixs_documents    = newDocs
                                                         }
                                  rnf newIxs `seq`
                                      return newIxs
    where
    nullContexts                = and . map (null . snd) $ rawContexts
    (did, newDocs)              = insertDoc (ixs_documents ixs) doc
    doc                         = Document
                                  { uri     = T.pack rawUri
                                  , desc    = rawDescription -- XXX: insert title?
                                  }

insertRawContextM               :: (Monad m, HolIndexM m i) =>
                                   DocId -> i -> (Context, [(Word, Position)]) -> m i
insertRawContextM did ix (cx, ws)
                                = foldM (insWordM cx did) ix ws

insWordM                        :: (Monad m, HolIndexM m i) =>
                                   Context -> DocId -> i -> (Word, Position) -> m i
insWordM cx' did' ix' (w', p')  = insertPositionM cx' w' did' p' ix'

-- ------------------------------------------------------------
