{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hunt.API
  ( -- API
    SearchAPI
  , CompletionAPI
  , DocumentAPI
  , EvalAPI
  , WeightAPI
  , SelectAPI
  , StatusAPI
  , IndexerAPI
  , HtmlAPI
  , HuntAPI

  , huntAPI

    -- Types
  , Offset
  , Limit
  , Suggestion
  ) where


import qualified Data.Text                as T
import qualified Hunt.ClientInterface     as HC
import           Hunt.Common.ApiDocument  (LimitedResult)
import           Hunt.Interpreter.Command (CmdResult (..), Command (..))
import           Hunt.Query.Intermediate  (RankedDoc)

import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html          (Html)


-- API

huntAPI :: Proxy HuntAPI
huntAPI = Proxy

-- | Hunt API
type HuntAPI =
       SearchAPI
  :<|> CompletionAPI
  :<|> DocumentAPI
  :<|> EvalAPI
  :<|> WeightAPI
  :<|> SelectAPI
  :<|> StatusAPI
  :<|> IndexerAPI
  :<|> HtmlAPI


type Offset = Int
type Limit  = Int

-- | Hunt search API, providing the following endpoints:
--
-- GET search/:query?offset=0&limit=10   Search with pagination
--
-- GET search/:query                     Search (unlimited # of results)
type SearchAPI =
        "search"
        :> Capture "query" T.Text
        :> QueryParam "offset" Offset
        :> QueryParam "limit"  Limit
        :> Get '[JSON] (LimitedResult RankedDoc)


type Suggestion = [(T.Text, HC.Score)]

-- | Hunt completion API, providing the following endpoints:
--
-- GET  completion/:query?limit=10       Word completions with limit.
type CompletionAPI =
        "completion"
        :> Capture "query" T.Text
        :> QueryParam "limit" Limit
        :> Get '[JSON] Suggestion


-- | Hunt document API, providing the following endpoints:
--
-- POST   /document                  Insert 'ApiDocument's.
--
-- PUT    /document                  Update 'ApiDocument's.
--
-- DELETE /document/:id              Delete documents by URI.
type DocumentAPI =
        "document"
        :> ReqBody '[JSON] HC.ApiDocument
        :> Post '[JSON] ()
   :<|> "document"
        :> ReqBody '[JSON] HC.ApiDocument
        :> Put '[JSON] ()
   :<|> "document"
        :> ReqBody '[JSON] HC.ApiDocument
        :> Delete '[JSON] ()


-- | Hunt Eval API, providing the following endpoints:
--
-- POST /eval                          Evaluates 'Command's.
type EvalAPI =
        "eval"
        :> ReqBody '[JSON] Command
        :> Post '[JSON] CmdResult


-- | Hunt WeightAPI, providing the following endpoints:
--
-- GET  /weight/:query               Search and return weights of documents
type WeightAPI =
        "weight"
        :> Capture "query" T.Text
        :> Get '[JSON] (LimitedResult RankedDoc)


-- | Hunt Select API, providing the following endpoints:
--
-- GET  /select/:query               Select raw without ordering (unlimited # of results)
type SelectAPI =
        "select"
        :> Capture "query" T.Text
        :> Get '[JSON] (LimitedResult RankedDoc)


-- | Hunt Status API, providing the following endpoints:
--
-- GET  /status/gc                    Garbage collection statistics.
--
-- GET  /status/doctable              JSON dump of the document table (/experimental/).
--
-- GET  /status/index                 JSON dump of the index (/experimental/).
--
-- GET  /status/context/:name         JSON dump of the context (/experimental/).
--
-- GET  /status/schema                JSON dump of the schema (/experimental/).
type StatusAPI =
        "status"
        :> "gc"
        :> Get '[JSON] CmdResult
   :<|> "status"
        :> "doctable"
        :> Get '[JSON] CmdResult
   :<|> "status"
        :> "index"
        :> Get '[JSON] CmdResult
   :<|> "status"
        :> "context"
        :> Capture "name" T.Text
        :> Get '[JSON] CmdResult


-- | Hunt Indexer API, providing the following endpoints:
--
-- POST /indexer/:filename       Store the index.
--
-- GET  /indexer/:filename       Load an index.
type IndexerAPI =
        "indexer"
        :> Capture "filename" T.Text
        :> Post '[JSON] ()
   :<|> "indexer"
        :> Capture "filename" T.Text
        :> Get '[JSON] ()


type HtmlAPI =
        "quickstart"
        :> Get '[HTML] Html
   :<|> Get '[HTML] Html

