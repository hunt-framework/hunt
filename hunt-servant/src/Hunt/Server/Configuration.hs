module Hunt.Server.Configuration
  ( HuntServerConfiguration (..)
  , defaultConfig
  ) where


import           System.Log.Logger (Priority (..))


-- | Hunt server configuration.
data HuntServerConfiguration = HuntServerConfiguration
  { huntServerHost     :: String          -- ^ The host.
  , huntServerPort     :: Int             -- ^ The port to use.
  , readIndexOnStartup :: Maybe FilePath  -- ^ Serialized index to load on startup.
  , logFile            :: FilePath        -- ^ Location of the logfile.
  , logPriority        :: Priority        -- ^ Priority level to log on stdout.
  } deriving (Show)


-- | Provide a default configuration for running the
-- server.
defaultConfig :: HuntServerConfiguration
defaultConfig = HuntServerConfiguration
  { huntServerHost = "*"
  , huntServerPort = 3000
  , readIndexOnStartup = Nothing
  , logPriority = DEBUG
  , logFile = "hunt.log"
  }
