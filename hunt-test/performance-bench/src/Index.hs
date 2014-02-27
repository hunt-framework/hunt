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

--import           Control.Applicative
--import           Control.Concurrent
import           Control.Monad

import qualified Data.Binary                  as Bin
--import           Data.Map                     (Map)
import qualified Data.Map                     as M
--import           Data.Maybe
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
--import qualified Data.ByteString              as BS
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

-- | wrapper type for index environment
data Hunt = Hunt { getHunt :: DefaultEnv }

-- | helper for easy initiation
initHunt :: IO Hunt
initHunt = do
  env <- initEnv emptyIndexer defaultRankConfig contextTypes
  return $ Hunt env

main :: IO ()
main = do
  hunt <- initEnv emptyIndexer defaultRankConfig contextTypes
  docs <- (getJson "./../data/random/RandomData.js" :: IO [ApiDocument])
  _ <- mapM (\c -> monitorCmd hunt $ InsertContext c (ContextSchema Nothing [] 1.0 True ctText)) [ "id", "context1", "context2", "contextdate", "contextgeo", "contextint" ]
  putStrLn "sequence with insertList:"
  monitorCmd hunt $ Sequence $ map Insert docs
  putStrLn "search word 'a' to check if everything evaluated"
  monitorCmd hunt $ Search (QWord QFuzzy "a") 1 1000
--  putStrLn "single command for each insert:"
--  start <- getCurrentTime
--  _ <- mapM (\doc -> monitorCmd hunt $ Insert doc) docs
--  end <- getCurrentTime
--  putStrLn $ "overall execution time: " ++ show (diffUTCTime end start)
  l <- getLine
  return ()

monitorCmd hunt cmd = do
  start <- getCurrentTime
  _ <- runCmd hunt cmd
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
-- Test
{-
typeName :: Typeable a => a -> String
typeName x
  = (tyConModule . fst . splitTyConApp $ t) ++ "." ++ show t
    where
    t = typeOf x

testIndex :: DataSet -> IndexAll -> IO ()
testIndex dataSet (IndexAll ix f) = do
  !apiDocs  <- getDataSet dataSet :: IO ApiDocuments
  --threadDelay (5 * 10^6)
  let docs = P.map (toDocAndWords' (tdSchema dataSet)) apiDocs
  start <- getCurrentTime
  ix'   <- foldM' (\i ((_d,w),did) -> addWordsIx f w did i) ix (zip docs [0..])
  end <- ix' `seq` getCurrentTime

  -- output
  ix' `seq` printStats Nothing ix' (diffUTCTime end start)
-}

-- ----------------------------------------------------------------------------
-- Utils

showF :: RealFloat a => a -> String
showF f = showFFloat (Just 2) f ""

printStats :: Typeable a => Maybe String -> a -> NominalDiffTime -> IO ()
printStats titleM x tdiff = do
  x `seq` performGC
  stats <- getGCStats
  sep
  -- who knew?
  _ <- T.sequence . fmap (putStrLn . (++) ">> ") $ titleM
--  putStrLn $ ">> " ++ typeName x
  ssep
  putStrLn $ "time: " ++ show tdiff
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

