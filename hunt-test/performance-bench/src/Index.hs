{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Numeric
import           Prelude                                 as P

import           Data.Default
import           GHC.Stats
import           GHC.Stats.Json                          ()
import           System.Mem

import           Control.DeepSeq

import           Data.Binary
import qualified Data.Text                               as T
import           Data.Time.Clock

import           Hunt.Common

import           Hunt.Interpreter

import           Data.Aeson
import qualified Data.ByteString.Lazy                    as BL

import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar

import qualified Hunt.Common.Document.Compression.BZip   as BZIP
import qualified Hunt.Common.Document.Compression.Snappy as SNAPPY
import qualified Hunt.DocTable                           as Dt
import           Hunt.DocTable.HashedDocTable

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  let jsonData = "./../data/random/RandomData.js"
--  let jsonData = "./hayoo.js"

  hunt <- initHunt :: IO (HuntEnv (Documents Document))
--  hunt <- initHunt :: IO (HuntEnv (Documents BZIP.CompressedDoc))
--  hunt <- initHunt :: IO (HuntEnv (Documents SNAPPY.CompressedDoc))
  showStats "Index initialized"
  -- read benchmark json data force evaluation with deepseq
  -- to be sure all data is read into memory before starting
  -- the actual benchmark
  docs <- (getJson jsonData :: IO [ApiDocument])
  showStats $ "JSON loaded and evaluated " ++ (show . head . show . head $!! docs)

  -- insert benchmark contexts
  _ <- run hunt $ Sequence [ InsertContext "context1" def
                           , InsertContext "context2" def
                           , InsertContext "context3" def
                           , InsertContext "context4" def
                           ]
  showStats "contexts created"

  -- benchmark insert performance
  _res <- runAndMonitor hunt $ Sequence (map Insert docs)
--  res <- runListAndMonitor hunt $ (map Insert docs)
  showStats "documents inserted"

  -- run query to check success
  _res <- runAndMonitor hunt $ Search (QWord QNoCase "a") 0 3000
  showStats "index used with search and garbage collected"
  _ <- getLine
  -- benchmark delete performance
  _res <- runAndMonitor hunt $ Sequence (map (\docId -> Delete . T.pack $ "rnd://" ++ show docId) [1..(length docs `div` 2)])
--  res <- runListAndMonitor hunt $ (map (\docId -> Delete . T.pack $ "rnd://" ++ show docId) [1..(length docs `div` 2)])
  showStats "documents removed"

  --l <- getLine
  return ()

runAndMonitor :: (Dt.DocTable dt, Binary dt) => HuntEnv dt -> Command -> IO ()
runAndMonitor hunt cmd = do
  start <- getCurrentTime
  _ <- run hunt cmd
  end <- getCurrentTime
  putStrLn $ "command execution time: " ++ show (diffUTCTime end start)
  return ()

runListAndMonitor :: (Dt.DocTable dt, Binary dt) => HuntEnv dt -> [Command] -> IO ()
runListAndMonitor hunt cmds = do
  start <- getCurrentTime
  _ <- mapM (\cmd -> run hunt cmd) cmds
  end <- getCurrentTime
  putStrLn $ "command execution time: " ++ show (diffUTCTime end start)
  return ()

-- ----------------------------------------------------------------------------
-- IO

getJson :: (FromJSON a) => FilePath -> IO a
getJson file = do
  content <- BL.readFile file
  case eitherDecode content of
    (Right x) -> return x
    (Left err) -> error err

-- ----------------------------------------------------------------------------
-- Utils

run :: (Dt.DocTable dt, Binary dt) => HuntEnv dt -> Command -> IO (Either CmdError CmdResult)
run = runCmd

withCmd :: (a -> b) -> a -> b
f `withCmd` x  =  f x


showF :: RealFloat a => a -> String
showF f = showFFloat (Just 2) f ""

showStats :: String -> IO ()
showStats t = do
  putStrLn t
  performGC
  performGC
  performGC
  performGC
  stats <- getGCStats
  putStrLn $ "Bytes used: " ++ (show $ currentBytesUsed stats) ++ " B"
