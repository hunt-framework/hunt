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

import           Hunt.Interpreter.Interpreter

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
  hunt <- getHunt (def :: DefaultHunt)
  -- read benchmark jsond data
  docs <- (getJson "./../data/random/RandomData.js" :: IO [ApiDocument])
  
  -- insert benchmark contexts
  run hunt `withCmd` Sequence [ InsertContext "context1" def
                              , InsertContext "context2" def
                              ]

  printStats (Just "Before Insert") ()
  getLine

  -- benchmark insert performance
  res <- runAndMonitor hunt `withCmd` Sequence (map Insert docs)
  printStats (Just "After Insert") res
  getLine

  -- run query to check success
  res <- runAndMonitor hunt `withCmd` Search (QWord QNoCase "a") 0 3000
  printStats (Just "After Search") (id $!! res)
  getLine

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
  return . fromRight . eitherDecode $ content

-- ----------------------------------------------------------------------------
-- Utils

run = runCmd

withCmd :: (a -> b) -> a -> b
f `withCmd` x  =  f x


showF :: RealFloat a => a -> String
showF f = showFFloat (Just 2) f ""

printStats :: Typeable a => Maybe String -> a -> IO ()
printStats titleM x = do
  x `seq` performGC
  stats <- getGCStats
  sep
  -- who knew?
  _ <- T.sequence . fmap (putStrLn . (++) ">> ") $ titleM
--  putStrLn $ ">> " ++ typeName x
  ssep
  printTop
  where
  sep  = putStrLn "==========="
  ssep = putStrLn "-----------"
  putStrLs = putStrLn . concat
  printGCStats :: ToJSON a => a -> IO ()
  printGCStats = B8.putStrLn . encodePretty' config
    where
    config = Config
      { confIndent  = 2
      , confCompare = keyOrder ["currentBytesUsed", "maxBytesUsed", "peakMegabytesAllocated"]
                        `mappend` compare
      }

printTop :: IO ()
printTop = do
  pid <- getProcessID
  res <- readProcess "top" ["-cbn", "1" ,"-p", show pid] []
  putStrLn res

