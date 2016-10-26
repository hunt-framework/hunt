{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Hunt.Server.API
  ( HuntServerAPI
  , HtmlAPI
  , huntServerAPI
  , module Hunt.API
  ) where

import           Hunt.API
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html    (Html)


-- SERVER API

-- | The HuntServerAPI provides additional capabilities
-- needed for serving the standard html client and introductions.
type HuntServerAPI
  = HuntAPI :<|> HtmlAPI


type HtmlAPI =
        "quickstart"
        :> Get '[HTML] Html
   :<|> Get '[HTML] Html


-- PROXY

huntServerAPI :: Proxy HuntServerAPI
huntServerAPI = Proxy

