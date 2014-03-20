{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Numeric
import           Prelude                      as P

import           GHC.Stats
import           GHC.Stats.Json               ()
import           System.Environment
import           System.FilePath
import           System.Mem
import           System.Posix.Process
import           System.Process
import           Data.Default

import           Control.Monad
import           Control.DeepSeq

import qualified Data.Binary                  as Bin
import qualified Data.Map                     as M
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock
import qualified Data.Traversable             as T
import           Data.Typeable

import           Hunt.Common

import           Hunt.Interpreter

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Char8   as B8

import           Hunt.Common.ApiDocument
import qualified Hunt.Common.Occurrences      as Occ
import           Hunt.Index.Schema.Analyze
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Ranking
import           Hunt.Utility

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  let json = "./../data/random/RandomData.js"
--  let json = "./hayoo.js"

  hunt <- initHunt :: IO DefHuntEnv
  showStats "Index initialized"
  -- read benchmark json data force evaluation with deepseq
  -- to be sure all data is read into memory before starting
  -- the actual benchmark
  docs <- (getJson json :: IO [ApiDocument])
  showStats $ "JSON loaded and evaluated " ++ (show . head . show . head $!! docs)

  -- insert benchmark contexts
  run hunt `withCmd` Sequence [ InsertContext "context1" def
                              , InsertContext "context2" def
                              , InsertContext "context3" def
                              , InsertContext "context4" def
                              ]
  showStats "contexts created"

  -- benchmark insert performance
  res <- runAndMonitor hunt `withCmd` Sequence (map Insert docs)
--  res <- runListAndMonitor hunt `withCmd` (map Insert docs)
  showStats "documents inserted"

  -- run query to check success
  res <- runAndMonitor hunt `withCmd` Search (QWord QNoCase "a") 0 3000
  showStats "index used with search and garbage collected"
  getLine
  -- benchmark delete performance
  res <- runAndMonitor hunt `withCmd` Sequence (map (\id -> Delete . T.pack $ "rnd://" ++ show id) [1..(length docs `div` 2)])
--  res <- runListAndMonitor hunt `withCmd` (map (\id -> Delete . T.pack $ "rnd://" ++ show id) [1..(length docs `div` 2)])
  showStats "documents removed"

  l <- getLine
  return ()

runAndMonitor hunt cmd = do
  start <- getCurrentTime
  run hunt `withCmd` cmd
  end <- getCurrentTime
  putStrLn $ "command execution time: " ++ show (diffUTCTime end start)
  return ()

runListAndMonitor hunt cmds = do
  start <- getCurrentTime
  mapM (\cmd -> run hunt `withCmd` cmd) cmds
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
