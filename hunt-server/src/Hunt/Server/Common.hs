{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE CPP                       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------
{- |
  Common data types and functions for the Hunt server.
-}
-- ----------------------------------------------------------------------------

module Hunt.Server.Common where

import           Data.Data              (Data)
import           Data.Default
import           Data.Typeable          (Typeable)

import           Data.Aeson

import           System.Console.CmdArgs (explicit, help, name, program, summary,
                                         typ, typFile, (&=))
import           System.Log.Logger      (Priority (..))


-- ------------------------------------------------------------

-- | Generic JSON response format.
data JsonResponse r f
  = ToJSON r => JsonSuccess     r -- ^ Successful response with the payload.
  | ToJSON f => JsonFailure Int f -- ^ Response that something went wrong.
                                  --   Includes an error code an a message.

instance ToJSON (JsonResponse r f) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure n msg) = object
    [ "code"  .= n
    , "msg"   .= msg
    ]

-- ------------------------------------------------------------
-- instances necessary for System.Console.CmdArgs
-- ------------------------------------------------------------
deriving instance Data     Priority
deriving instance Typeable Priority

-- ------------------------------------------------------------

-- | Hunt server configuration.
data HuntServerConfiguration = HuntServerConfiguration
  { huntServerHost     :: String          -- ^ The host.
  , huntServerPort     :: Int             -- ^ The port to use.
  , readIndexOnStartup :: Maybe FilePath  -- ^ Serialized index to load on startup.
  , logFile            :: FilePath        -- ^ Location of the logfile.
  , logPriority        :: Priority        -- ^ Priority level to log on stdout.
  , statsDHost         :: String          -- ^ statsd host name
  , statsDPort         :: Int             -- ^ statsd port
  } deriving (Show, Data, Typeable)


instance Default HuntServerConfiguration where
  def = HuntServerConfiguration {
    huntServerHost = "*"
      &= explicit &= name "host"
      &= help "Bind to altenate host (* = any, *4 = any IPv4, *6 = any IPv6)",
    huntServerPort = (3000::Int)
      &= explicit &= name "port"
      &= help "Listen on alternate port",
    readIndexOnStartup = Nothing
      &= explicit &= name "load-index" &= typFile
      &= help "Load index on startup",
    logPriority = DEBUG
      &= explicit &= name "log-priority" &= typ "PRIORITY"
      &= help "Set log level for stdout",
    logFile = "hunt.log"
      &= explicit &= name "log-file" &= typFile
      &= help "Set logfile location"
#ifdef SUPPORT_STATSD
    , statsDHost = ""
      &= explicit &= name "statsd-host"
      &= help "statsd host name",
    statsDPort = (8125 :: Int)
      &= explicit &= name "statsd-port"
      &= help "statsd port"
#endif
  } &= summary "Standalone search server based on the Hunt searchengine."
    &= program "hunt-server"

-- ------------------------------------------------------------
