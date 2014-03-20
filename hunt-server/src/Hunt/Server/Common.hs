{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs              #-}

module Hunt.Server.Common where

import           Data.Data         (Data)
import           Data.Typeable     (Typeable)

import           Data.Aeson

import           System.Log.Logger (Priority(..))

-- ----------------------------------------------------------------------------

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

-- instances necessary for System.Console.CmdArgs
deriving instance Data     Priority
deriving instance Typeable Priority

-- | Hunt server configuration.
data HuntServerConfiguration = HuntServerConfiguration
  { huntServerHost     :: String          -- ^ The host.
  , huntServerPort     :: Int             -- ^ The port to use.
  , readIndexOnStartup :: Maybe FilePath  -- ^ A serialized index to load on startup.
  , logFile            :: FilePath        -- ^ The location of the logfile.
  , logPriority        :: Priority        -- ^ The priority level to log on stdout.
  } deriving (Show, Data, Typeable)
