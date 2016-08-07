{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- ------------------------------------------------------------

module Hunt.Utility.Output
where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as LB
import           Data.Text                ()
import           System.FilePath          ()

-- ------------------------------------------------------------

--
outputValue :: (Functor m, MonadIO m, ToJSON c) => String -> c -> m ()
outputValue fn c
    = liftIO (jsonOutput True toFile c)
    where
      toFile bs
          | null fn
            ||
            fn == "-"
                = LB.putStr bs
          | otherwise
              = LB.writeFile fn bs

-- TODO: merge with Hunt.Server.Client.handleJsonResponse
--evalOkRes :: MonadIO m => Maybe LB.ByteString -> m ()
--evalOkRes Nothing
--    = return ()
--evalOkRes (Just bs)
--    | isOkMsg bs = return ()
--    | otherwise  = liftIO . ioError . userError $
--                  "server error: \"ok\" expected, but got " ++ show (LC.unpack bs)
--    where
--      isOkMsg s = maybe False ((== "ok") . unCmdRes) js
--          where
--            js :: Maybe (CmdRes Text)
--            js = decode s

--evalErrRes :: MonadIO m => Int -> LB.ByteString -> m a
--evalErrRes rc bs
--    = liftIO . ioError . userError $
--      unwords ["server error: rc=", show rc, "msg=", msg ce]
--    where
--      ce :: Maybe CmdError
--      ce = decode bs

--      msg Nothing  = "result is not a JSON error message"
--      msg (Just e) = show e

-- ------------------------------------------------------------

jsonOutput :: (ToJSON c) => Bool -> (LB.ByteString -> IO a) -> c -> IO a
jsonOutput pretty io x
    = io $ (if pretty then encodePretty' encConfig else encode) x
      where
#if MIN_VERSION_aeson_pretty(0, 8, 0)
        indent = Spaces 2
#else
        indent = 2
#endif

        encConfig :: Config
        encConfig
            = Config { confIndent = indent
                     , confCompare
                         = keyOrder ["description", "index", "uri"]
                           `mappend`
                           compare
                     }

-- ------------------------------------------------------------
